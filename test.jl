@use "." => DOM Container Text diff @dom
@use "./css.jl" @css_str CSSNode
@use "./html.jl" @html_str
@use "./ansi.jl" ansi
@use Test: @testset, @test
@use Crayons: Box

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
  @test @dom[:path stroke-width="2"].attrs == Dict{Symbol,Any}(:strokeWidth=>"2")
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
  @test repr("text/html", @dom[:img style.background-color="red"]) == "<img style=\"background-color:red;\"/>"
  @test repr("text/html", @dom[:a style.borderWidth=3 "b"]) == "<a style=\"border-width:3;\">b</a>"
end

@testset "show(::IO, ::MIME\"application/json\", ::Node)" begin
  @test repr("application/json", @dom[:a "b"]) == """{"tag":"a","attrs":{},"children":[{"type":"Text","value":"b"}]}"""
  @test repr("application/json", @dom[:a class="a"]) == """{"tag":"a","attrs":{"class":["a"]}}"""
  @test repr("application/json", @dom[:a class="a" "b"]) == """{"tag":"a","attrs":{"class":["a"]},"children":[{"type":"Text","value":"b"}]}"""
  @test repr("application/json", @dom[:a aB=true]) ==   """{"tag":"a","attrs":{"a-b":true}}"""
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
           CSSNode(Dict{Symbol,Any}(:width=>"500px",
                        Symbol("align-self")=>"flex-start",
                        Symbol("margin-top")=>"100px"),
                   Dict{Vector{String},CSSNode}()))
  check(css"""
        a, b
          color: red
          > c, &:first-child
            color: blue
        """, r"\.\w+ a,\.\w+ b{color:red;}\.\w+ a > c,\.\w+ b > c,\.\w+ a:first-child,\.\w+ b:first-child{color:blue;}")
  check(css"svg {stroke: rgb(255, 65, 65); transform: rotate(180deg)}",
        r"\.\w+ svg{stroke:rgb\(255, 65, 65\);transform:rotate\(180deg\);}")
  check(css"&:hover {color: red}", r"\.\w+:hover{color:red;}")
  check(css":is(a,b), c {color: red}", r"\.[_\w]+ :is\(a,b\),\.[_\w]+ c{color:red;}")
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

@use "./stdlib/main.jl" dom seperate vstack hstack expandable brief syntax fade
@use "./stdlib/Table.jl" table header body footer row
@use "./stdlib/Breadcrumb.jl" breadcrumb
@use "./stdlib/ButtonGroup.jl" button button_group
@use "./stdlib/Range.jl" slider
@use "./stdlib/Markdown.jl" renderMD
import Markdown
import Dates

@testset "dom() basic types" begin
  @test dom(42) isa Container{:span}
  @test dom(3.14) isa Container{:span}
  @test dom(true) isa Container{:span}
  @test dom(:hello) isa Container{:span}
  @test dom('x') isa Container{:span}
  @test dom(nothing) isa Container{:span}
  @test dom(missing) isa Container{:span}
  @test dom(r"abc") isa Container{:span}
  @test dom(v"1.2.3") isa Container{:span}
end

@testset "dom() strings" begin
  @test dom("hello") isa Container{:span}
  multiline = dom("line1\nline2")
  @test multiline isa Container{:div}
end

@testset "dom() dates" begin
  @test dom(Dates.Date(2024, 1, 15)) isa Container{:span}
  @test dom(Dates.Time(12, 30)) isa Container{:span}
end

@testset "dom() rationals" begin
  @test dom(1//3) isa Container{:span}
  @test dom(7//3) isa Container{:span}
end

@testset "dom() pairs" begin
  node = dom(:a => 1)
  @test node isa DOM.Node
  html = repr("text/html", node)
  @test occursin("a", html) && occursin("1", html)
end

@testset "dom() ranges" begin
  @test dom(1:10) isa Container{:span}
  @test dom(1:2:10) isa Container{:span}
  @test occursin("1:10", repr("text/html", dom(1:10)))
end

@testset "dom() complex" begin
  node = dom(3 + 4im)
  @test node isa Container{:span}
  html = repr("text/html", node)
  @test occursin("im", html)
end

@testset "dom() expr" begin
  node = dom(:(x + y))
  @test node isa Container{:span}
  @test occursin("x", repr("text/html", node))
end

@testset "dom() exceptions" begin
  node = dom(ErrorException("boom"))
  @test node isa Container{:span}
  @test occursin("boom", repr("text/html", node))
end

@testset "dom() regexmatch" begin
  m = match(r"(\d+)", "abc123")
  node = dom(m)
  @test node isa Container{:div}
  html = repr("text/html", node)
  @test occursin("123", html)
end

@testset "dom() cmd" begin
  node = dom(`echo hello`)
  @test node isa Container{:span}
  @test occursin("echo", repr("text/html", node))
end

@testset "dom() some" begin
  node = dom(Some(42))
  @test node isa Container{:span}
  @test occursin("Some", repr("text/html", node))
end

@testset "dom() periods" begin
  @test dom(Dates.Day(5)) isa Container{:span}
  @test occursin("Day", repr("text/html", dom(Dates.Day(5))))
end

@testset "dom() abstractset" begin
  bs = BitSet([1, 3, 5])
  @test dom(bs) isa Container{:details}
  @test dom(BitSet()) isa Container{:span}
end

@testset "dom() collections" begin
  @test dom(Dict(:a=>1)) isa Container{:details}
  @test dom(Dict()) isa Container{:span}
  @test dom([1,2,3]) isa Container{:details}
  @test dom(Int[]) isa Container{:span}
  @test dom(Set([1,2])) isa Container{:details}
  @test dom((1,2,3)) isa Container{:div}
  @test dom((a=1, b=2)) isa Container{:div}
end

@testset "dom() types" begin
  @test dom(Int) isa Container{:span}
  @test dom(Vector{Int}) isa Container{:details}
  @test dom(Union{Int,String}) isa Container{:span}
end

@testset "dom() matrix" begin
  @test dom([1 2; 3 4]) isa Container{:table}
end

@testset "dom() functions" begin
  @test dom(sin) isa Container{:details}
  @test dom(first(methods(sin))) isa Container{:span}
end

@testset "dom() markdown" begin
  md = Markdown.parse("# Hello\n\nA paragraph")
  @test dom(md) isa Container{:div}
end

@testset "dom() generic struct" begin
  struct TestPoint; x::Int; y::Int end
  @test dom(TestPoint(1, 2)) isa Container{:details}
end

@testset "seperate" begin
  node = seperate(1234567)
  @test repr("text/html", node) |> s -> occursin("1", s)
  @test repr("text/html", node) |> s -> occursin("234", s)
end

@testset "table" begin
  t = @dom[table
    [header "Name" "Value"]
    [body [row "a" "1"] [row "b" "2"]]]
  @test t isa Container{:div}
  html = repr("text/html", t)
  @test occursin("<table>", html)
  @test occursin("<thead>", html)
  @test occursin("<tbody>", html)
end

@testset "breadcrumb" begin
  bc = @dom[breadcrumb "Home" "Folder" "File"]
  @test bc isa Container{:nav}
  html = repr("text/html", bc)
  @test occursin("<ol", html)
  @test occursin("Home", html)
end

@testset "button_group" begin
  bg = @dom[button_group "A" "B" "C"]
  @test bg isa Container{:div}
  html = repr("text/html", bg)
  @test occursin("<button", html)
  @test occursin("A", html)
end

@testset "slider" begin
  s = @dom[slider]
  @test s isa Container{:div}
  html = repr("text/html", s)
  @test occursin("type=\"range\"", html)
end

@testset "renderMD" begin
  md = Markdown.parse("**bold** and *italic*")
  node = renderMD(md.content[1])
  html = repr("text/html", node)
  @test occursin("<b>", html)
  @test occursin("<em>", html)
end
