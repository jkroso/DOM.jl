#! /usr/bin/env jest
@require "." => DOM Container Text diff @dom
@require "./style" @css @css_str CSSNode

testset("@dom [<tag> <attr>... <child>...]") do
  @test @dom("a") == Text("a")
  @test @dom([:div class="a"]) == Container{:div}(Dict(:class=>Set([:a])), [])
  @test @dom([:div class-a=true]) == Container{:div}(Dict(:class=>Set([:a])), [])
  @test @dom([:div class-a=false]) == Container{:div}(Dict(:class=>Set([])), [])
  @test isa(macroexpand(:(@dom "a")), Text)
  @test isa(macroexpand(:(@dom [:div class="a"])), Container)
  avariable = "some text"
  @test @dom([:p avariable]) == Container{:p}(Dict(),[Text("some text")])
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
  @test sprint(show, "text/css", @css [color: :blue]) == "5f866d8b9d5c1eee{color:blue;}"
  @test sprint(show, "text/css", @css [div: [color: :blue]]) == "b2d84196cd68fc94 div{color:blue;}"
  @test ==(sprint(show, "text/css", @css [color: :blue div: [color: :red]]),
           "45e7528c51dda7ac{color:blue;}45e7528c51dda7ac div{color:red;}")
  @test ==(sprint(show, "text/css", convert(CSSNode, [:background => :red,
                                                      "> div" => [:color => :blue]])),
           "bc6561a55084f1de{background:red;}bc6561a55084f1de > div{color:blue;}")
  @test sprint(show, "text/css", css"""
        background: red
        > div:
          color: blue
        color: black
        """) == "9c8bc60a0761f869{background:red;color:black;}9c8bc60a0761f869 > div{color:blue;}"
end
