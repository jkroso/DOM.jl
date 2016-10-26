@require "." Container diff Patch UpdateText Node
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

const global_sheet = Container{:style}(Dict(), [StyleSheet(Set{CSSNode}())])

macro css_str(string)
  node = parse_css(IOBuffer(string))
  push!(global_sheet.children[1].styles, node)
  "_" * (hash(node) |> hex)
end
