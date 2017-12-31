#! /usr/bin/env jest
@require "." => DOM Container Text diff @dom emit
@require "./css" @css_str CSSNode
@require "./Events" => Events

testset("@dom [<tag> <attr>... <child>...]") do
  @test @dom("a") == Text("a")
  @test @dom([:div class=:a]) == Container{:div}(Dict{Symbol,Any}(:class=>Set([:a])), [])
  @test @dom([:div class="a"]) == Container{:div}(Dict{Symbol,Any}(:class=>Set([:a])), [])
  @test @dom([:div class.a=true]) == Container{:div}(Dict{Symbol,Any}(:class=>Set([:a])), [])
  @test @dom([:div class.a=false]) == Container{:div}(Dict{Symbol,Any}(), [])
  @test isa(macroexpand(:(@dom "a")), Text)
  # Worth optimizing?
  # @test isa(macroexpand(:(@dom [:div class="a"])), Container)
  avariable = "some text"
  @test @dom([:p avariable]) == Container{:p}(Dict{Symbol,Any}(),[Text("some text")])
  @test ==(@dom([:html
                  [:body class="a"
                    [:ul]]]),
           @dom([:html [:body class="a" [:ul]]]))
  a(attrs, children) = @dom [:a class=attrs[:class]]
  @test @dom([a class=:a]) == @dom([:a class=:a])
  b = :class=>:a
  c = @dom [:p]
  @test @dom([:a vcat(c)...]) == Container{:a}(Dict{Symbol,Any}(),[c])
  @test @dom([:a{[b]...}]) == Container{:a}(Dict{Symbol,Any}(:class=>Set([:a])),[])
  @test @dom([:a{b} [:b]]) == Container{:a}(Dict{Symbol,Any}(:b=>Dict{Symbol,Any}(:class=>:a)),
                                            [Container{:b}(Dict{Symbol,Any}(),[])])
  @test @dom([:a a=true a=false]).attrs == Dict(:a=>false)
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
  @test stringmime("application/json", @dom [:a "b"]) == """{"tag":"a","attrs":{},"children":[{"type":"Text","value":"b"}]}"""
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
  @test diff(@dom([:div style.height="1px"]),
             @dom([:div style.height="0px"]))|>get == DOM.Mutation([DOM.UpdateStyle([], [:height=>"0px"])], [])
end

testset("style") do
  check(n::CSSNode, css::Regex) = @test ismatch(css, stringmime("text/css", n))
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

testset( "emit(::Node,::Event)") do
  n = 0
  spy(e::Events.Mouse) = n += 1
  spy(e::Number) = n += e
  tree = @dom [:div onmouseover=spy]
  emit(tree, Events.MouseOver([]))
  @test n == 1
  emit(Container[tree], :onmouseover, 1)
  @test n == 2
  tree = @dom [:p [:p onmouseover=spy]]
  emit(tree, Events.MouseOver([1]))
  @test n == 3
  tree = @dom [:p onmouseover=e->(@test(n==4);n+=1) [:p onmouseover=spy]]
  emit(tree, Events.MouseOver([1]))
  @test n == 5
end
