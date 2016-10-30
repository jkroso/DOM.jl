#! /usr/bin/env jest
@require "." => DOM Container Text diff @dom dispatch
@require "./css" @css @css_str CSSNode
@require "./Events" => Events

testset("@dom [<tag> <attr>... <child>...]") do
  @test @dom("a") == Text("a")
  @test @dom([:div class="a"]) == Container{:div}(Dict(:class=>Set([:a])), [])
  @test @dom([:div class-a=true]) == Container{:div}(Dict(:class=>Set([:a])), [])
  @test @dom([:div class-a=false]) == Container{:div}(Dict(:class=>Set([])), [])
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
end

testset("style") do
  @test hash(@css([color: :blue])) == hash(@css([color: :blue]))
  @test sprint(show, "text/css", @css [color: :blue]) == "._5f866d8b9d5c1eee{color:blue;}"
  @test sprint(show, "text/css", @css [div: [color: :blue]]) == "._b2d84196cd68fc94 div{color:blue;}"
  @test ==(sprint(show, "text/css", @css [color: :blue div: [color: :red]]),
           "._45e7528c51dda7ac{color:blue;}._45e7528c51dda7ac div{color:red;}")
  @test ==(sprint(show, "text/css", convert(CSSNode, [:background => :red,
                                                      "> div" => [:color => :blue]])),
           "._bc6561a55084f1de{background:red;}._bc6561a55084f1de > div{color:blue;}")
  @test sprint(show, "text/css", css"""
        background: red
        > div:
          color: blue
        color: black
        """) == "._9c8bc60a0761f869{background:red;color:black;}._9c8bc60a0761f869 > div{color:blue;}"
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
