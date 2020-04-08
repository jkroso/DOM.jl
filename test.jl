@use "github.com/KristofferC/Crayons.jl" Box
@use "." => DOM Container Text diff @dom
@use "./css.jl" @css_str CSSNode
@use "./html.jl" @html_str
@use "./ansi.jl" ansi
using Test

@testset "@dom[<tag> <attr>... <child>...]" begin
  @test @dom("a") == Text("a")
  @test @dom[:div class=:a] == Container{:div}(Dict{Symbol,Any}(:class=>Set([:a])), [])
  @test @dom[:div class="a"] == Container{:div}(Dict{Symbol,Any}(:class=>Set([:a])), [])
  @test @dom[:div class="a b"] == Container{:div}(Dict{Symbol,Any}(:class=>Set([:a, :b])), [])
  b = "b"
  @test @dom[:div class="a $b"] == Container{:div}(Dict{Symbol,Any}(:class=>Set([:a, :b])), [])
  @test @dom[:div class.a=true] == Container{:div}(Dict{Symbol,Any}(:class=>Set([:a])), [])
  @test @dom[:div class.a=false] == Container{:div}(Dict{Symbol,Any}(), [])
  @test isa(@macroexpand(@dom "a"), Text)
  # Worth optimizing?
  # @test isa(macroexpand(:(@dom[:div class="a"])), Container)
  avariable = "some text"
  @test @dom[:p avariable] == Container{:p}(Dict{Symbol,Any}(),[Text("some text")])
  @test ==(@dom[:html
                  [:body class="a"
                    [:ul]]],
           @dom[:html [:body class="a" [:ul]]])
  a(attrs, children) = @dom[:a class=attrs[:class]]
  @test @dom[a class=:a] == @dom[:a class=:a]
  b = :class=>:a
  c = @dom[:p]
  @test @dom[:a vcat(c)...] == Container{:a}(Dict{Symbol,Any}(),[c])
  @test @dom[:a{[b]...}] == Container{:a}(Dict{Symbol,Any}(:class=>Set([:a])),[])
  @test @dom[:a{b} [:b]] == Container{:a}(Dict{Symbol,Any}(:b=>Dict{Symbol,Any}(:class=>:a)),
                                            [Container{:b}(Dict{Symbol,Any}(),[])])
  @test @dom[:a a=true a=false].attrs == Dict(:a=>false)
  @test @dom[:div focus=true class=:a] == Container{:div}(Dict{Symbol,Any}(:focus=>true,:class=>Set([:a])),[])
end

@testset "show(::IO, ::MIME\"text/html\", ::Node)" begin
  @test repr("text/html", @dom[:a "b"]) == "<a>b</a>"
  @test repr("text/html", @dom[:div class="a" [:a "b"]]) == "<div class=\"a\"><a>b</a></div>"
  @test repr("text/html", @dom[:img]) == "<img/>"
  @test repr("text/html", @dom[:span "<>"]) == "<span>&lt;&gt;</span>"
  doc = @dom[:html
    [:head [:style "body > p {color: red}"]]
    [:body [:p "Loading"]]]
  @test repr("text/html", doc) == "<html><head><style>body > p {color: red}</style></head><body><p>Loading</p></body></html>"
  @test repr("text/html", @dom[:img style.backgroundColor="red"]) == "<img style=\"background-color:red;\"/>"
  @test repr("text/html", @dom[:a style.borderWidth=3 "b"]) == "<a style=\"border-width:3;\">b</a>"
end

@testset "show(::IO, ::MIME\"application/json\", ::Node)" begin
  @test repr("application/json", @dom[:a "b"]) == """{"tag":"a","attrs":{},"children":[{"type":"Text","value":"b"}]}"""
  @test repr("application/json", @dom[:a class="a"]) == """{"tag":"a","attrs":{"class":["a"]}}"""
  @test repr("application/json", @dom[:a class="a" "b"]) == """{"tag":"a","attrs":{"class":["a"]},"children":[{"type":"Text","value":"b"}]}"""
end

@testset "diff" begin
  @test diff(@dom("a"), @dom("b")) == DOM.UpdateText("b")
  @test diff(@dom[:p "a"], @dom[:p "b"]) == DOM.Mutation([], [DOM.UpdateText("b")])
  @test ==(diff(@dom[:html [:body [:p "a"]]], @dom[:html [:body [:p "b"]]]),
           DOM.Mutation([],[DOM.Mutation([],[DOM.Mutation([],[DOM.UpdateText("b")])])]))
  @test ==(diff(@dom[:a [:b] [:b "a"]], @dom[:a [:b] [:b "b"]]),
           DOM.Mutation([],[DOM.Skip(1),DOM.Mutation([],[DOM.UpdateText("b")])]))
  @test diff(@dom[:a 1], @dom[:a 1 2]) == DOM.Mutation([],[DOM.AppendChildren([2])])
  @test diff(@dom[:a 1 2], @dom[:a 1]) == DOM.Mutation([],[DOM.TrimChildren(1)])
  @test diff(@dom[:div style.height="1px"],
             @dom[:div style.height="0px"]) == DOM.Mutation([DOM.UpdateStyle([], [:height=>"0px"])], [])
end

@testset "style" begin
  check(n::CSSNode, css::Regex) = @test occursin(css, repr("text/css", n))
  @test hash(css"color: blue") == hash(css"color: blue")
  check(css"color: blue", r"\.\w+{color:blue;}")
  check(css"""
           div
             color: blue
           """, r"\.\w+ div{color:blue;}")
  check(css"""
           color: blue
           div
             color: red
           """, r"\.\w+{color:blue;}\.\w+ div{color:red;}")
  check(css"""
        background: red
        > div
          color: blue
        color: black
        """, r"\.\w+{color:black;background:red;}\.\w+ > div{color:blue;}")
  check(css"""
        background: red
        &:first-child
          color: blue
        color: black
        """, r"\.\w+{color:black;background:red;}\.\w+:first-child{color:blue;}")
  @test ==(css"width: 500px; align-self: flex-start; margin-top: 100px",
           CSSNode(Dict(:width=>"500px",
                        Symbol("align-self")=>"flex-start",
                        Symbol("margin-top")=>"100px"),
                   Dict()))
  check(css"""
        a, b
          color: red
          > c, &:first-child
            color: blue
        """, r"\.\w+ a,\.\w+ b{color:red;}\.\w+ a > c,\.\w+ b > c,\.\w+ a:first-child,\.\w+ b:first-child{color:blue;}")
  check(css"svg {stroke: rgb(255, 65, 65); transform: rotate(180deg)}",
        r"\.\w+ svg{stroke:rgb\(255, 65, 65\);transform:rotate\(180deg\);}")
  check(css"&:hover {color: red}", r"\.\w+:hover{color:red;}")
end

@testset "parse(::MIME\"text/html\", data)" begin
  @test html"<a class=a>ab<span b>cd</span></a>" == @dom[:a class="a" "ab" [:span b=true "cd"]]
  @test html"<a class=a><!-- abc --></a>" == @dom[:a class="a"]
  @test html"<a class=a><!--abc--></a>" == @dom[:a class="a"]
  @test html"<div style=\"color: red; background: blue;\"></div>" == @dom[:div style.color="red" style.background="blue"]
  @test html"<div style=\"background-color: blue\"></div>" == @dom[:div style.backgroundColor="blue"]
  @test html"<div ><img /> </div>" == @dom[:div [:img] " "]
  @test html"<font=\"monospace\" >a</font>" == @dom[:font font="monospace" "a"]
  @test html"<p>&amp; &#97; &x23;</p>" == @dom[:p "& a #"]
  @test html"<p>&nbsp;&nbsp;</p>" == @dom[:p "$(Char(160))$(Char(160))"]
  @test html"<a class=\"a b\"/>" == @dom[:a class="a b"]
end

@testset "ansi(str)" begin
  @test ansi(string(Box.CYAN_FG)) == @dom[:p [:span style.color="lightgray" ""]
                                             [:span style.color="cyan" ""]]
end
