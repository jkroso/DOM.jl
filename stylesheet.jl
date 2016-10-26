@require "." Container diff Patch UpdateText Node Text
@require "./css" parse_css CSSNode

immutable StyleSheet <: Node
  styles::Set{CSSNode}
end

Base.show(io::IO, m::MIME"text/html", n::StyleSheet) =
  for s in n.styles
    show(io, "text/css", s)
  end

Base.show(io::IO, m::MIME"application/json", n::StyleSheet) = begin
  write(io, b"""{"type":"Text","value":""")
  show(io, m, sprint(show, MIME("text/html"), n))
  write(io, '}')
  nothing
end

diff(a::StyleSheet, b::StyleSheet) = begin
  atxt = sprint(show, MIME("text/html"), a)
  btxt = sprint(show, MIME("text/html"), b)
  Nullable{Patch}(atxt == btxt ? nothing : UpdateText(btxt))
end

macro css_str(string)
  node = parse_css(IOBuffer(string))
  push!(global_sheet.children[1].styles, node)
  "_" * (hash(node) |> hex)
end

const global_sheet = Container{:style}(Dict(), [StyleSheet(Set{CSSNode}())])
const reset = Container{:style}(Dict(), [Text("html,body,p,ol,ul,li,dl,dt,dd,blockquote,figure,fieldset,legend,textarea,pre,iframe,hr,h1,h2,h3,h4,h5,h6{margin:0;padding:0}body{height:100%;width:100%;position:absolute}h1,h2,h3,h4,h5,h6{font-size:100%;font-weight:normal}ul{list-style:none}button,input,select,textarea{margin:0}html{box-sizing:border-box}*{box-sizing:inherit}*:before,*:after{box-sizing:inherit}img,embed,object,audio,video{height:auto;max-width:100%}iframe{border:0}table{border-collapse:collapse;border-spacing:0}td,th{padding:0;text-align:left}")])
