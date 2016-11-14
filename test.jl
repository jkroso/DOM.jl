#! /usr/bin/env jest
@require "." => DOM Container Text diff @dom dispatch
@require "./css" @css_str CSSNode
@require "./Events" => Events

testset("@dom [<tag> <attr>... <child>...]") do
  @test @dom("a") == Text("a")
  @test @dom([:div class=:a]) == Container{:div}(Dict(:class=>Set([:a])), [])
  @test @dom([:div class="a"]) == Container{:div}(Dict(:class=>Set([:a])), [])
  @test @dom([:div class.a=true]) == Container{:div}(Dict(:class=>Set([:a])), [])
  @test @dom([:div class.a=false]) == Container{:div}(Dict(), [])
  @test isa(macroexpand(:(@dom "a")), Text)
  @test isa(macroexpand(:(@dom [:div class="a"])), Container)
  avariable = "some text"
  @test @dom([:p avariable]) == Container{:p}(Dict(),[Text("some text")])
  @test ==(@dom([:html
                  [:body class="a"
                    [:ul]]]),
           @dom([:html [:body class="a" [:ul]]]))
  a(attrs, children) = @dom [:a class=attrs[:class]]
  @test @dom([a class=:a]) == @dom([:a class=:a])
  b = :class=>:a
  @test @dom([:a b=1 b]) == Container{:a}(Dict(:b=>1,:class=>Set([:a])),[])
  c = @dom [:p]
  @test @dom([:a c b]) == Container{:a}(Dict(:class=>Set([:a])),[c])
  @test @dom([:a vcat(c)...]) == Container{:a}(Dict(),[c])
  @test @dom([:a vcat(b)...]) == Container{:a}(Dict(:class=>Set([:a])),[])
  @test @dom([:a [:b] b]) == Container{:a}(Dict(:class=>Set([:a])),[Container{:b}(Dict(),[])])
end

testset("show(::IO, ::MIME\"text/html\", ::Node)") do
  @test stringmime("text/html", @dom [:a "b"]) == "<a>b</a>"
  @test stringmime("text/html", @dom [:div class="a" [:a "b"]]) == "<div class=\"a\"><a>b</a></div>"
  @test stringmime("text/html", @dom [:img]) == "<img/>"
  doc = @dom [:html
    [:head [:style "p {color: red}"]]
    [:body [:p "Loading"]]]
  @test ==(stringmime("text/html", doc),
           "<html><head><style>p {color: red}</style></head><body><p>Loading</p></body></html>")
end

testset("show(::IO, ::MIME\"application/json\", ::Node)") do
  @test stringmime("application/json", @dom [:a "b"]) == """{"tag":"a","children":[{"type":"Text","value":"b"}]}"""
  @test stringmime("application/json", @dom [:a class="a"]) == """{"tag":"a","attrs":{"class":["a"]}}"""
  @test stringmime("application/json", @dom [:a class="a" "b"]) == """{"tag":"a","attrs":{"class":["a"]},"children":[{"type":"Text","value":"b"}]}"""
end

testset("diff") do
  @test diff(@dom("a"), @dom("b"))|>get == DOM.UpdateText("b")
  @test diff(@dom([:p "a"]), @dom([:p "b"]))|>get == DOM.Mutation([], [DOM.UpdateText("b")])
  @test ==(diff(@dom([:html [:body [:p "a"]]]), @dom([:html [:body [:p "b"]]]))|>get,
           DOM.Mutation([],[DOM.Mutation([],[DOM.Mutation([],[DOM.UpdateText("b")])])]))
  @test ==(diff(@dom([:a [:b] [:b "a"]]), @dom([:a [:b] [:b "b"]]))|>get,
           DOM.Mutation([],[DOM.Skip(1),DOM.Mutation([],[DOM.UpdateText("b")])]))
  @test diff(@dom([:a 1]), @dom([:a 1 2]))|>get == DOM.Mutation([],[DOM.AppendChildren([2])])
  @test diff(@dom([:a 1 2]), @dom([:a 1]))|>get == DOM.Mutation([],[DOM.TrimChildren(1)])
end

testset("style") do
  @test hash(css"color: blue") == hash(css"color: blue")
  @test stringmime("text/css", css"color: blue") == "._318be16f33df2387{color:blue;}"
  @test stringmime("text/css", css"""
                                  div
                                    color: blue
                                  """) == "._edc3064e36b8ec38 div{color:blue;}"
  @test ==(stringmime("text/css", css"""
                                     color: blue
                                     div
                                       color: red
                                     """),
           "._b2ed6f1d54cfd80d{color:blue;}._b2ed6f1d54cfd80d div{color:red;}")
  @test stringmime("text/css", css"""
        background: red
        > div
          color: blue
        color: black
        """) == "._c34baaebdb3df5db{background:red;color:black;}._c34baaebdb3df5db > div{color:blue;}"
  @test stringmime("text/css", css"""
        background: red
        &:first-child
          color: blue
        color: black
        """) == "._f80d2350ffe458a3{background:red;color:black;}._f80d2350ffe458a3:first-child{color:blue;}"
  @test ==(css"width: 500px; align-self: flex-start; margin-top: 100px",
           CSSNode(Dict(:width=>"500px",
                        Symbol("align-self")=>"flex-start",
                        Symbol("margin-top")=>"100px"),
                   Dict()))
  @test stringmime("text/css", css"""
        a, b
          color: red
          > c, &:first-child
            color: blue
        """) == "._94c613663ecca099 a,._94c613663ecca099 b{color:red;}._94c613663ecca099 a > c,._94c613663ecca099 b > c,._94c613663ecca099 a:first-child,._94c613663ecca099 b:first-child{color:blue;}"
  @test ==(stringmime("text/css", css"svg {stroke: rgb(255, 65, 65); transform: rotate(180deg)}"),
           "._9a660138489d4520 svg{transform:rotate(180deg);stroke:rgb(255, 65, 65);}")
end

testset( "dispatch(::Node,::Event)") do
  n = 0
  spy(e::Events.Focus) = n += 1
  tree = @dom [:div onfocus=spy]
  dispatch(tree, Events.Focus([]))
  @test n == 1
  tree = @dom [:p [:p onfocus=spy]]
  dispatch(tree, Events.Focus([1]))
  @test n == 2
end
