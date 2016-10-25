@require "./css" parse_css CSSNode
@require "." Node

immutable StyleSheet <: Node
  styles::Set{CSSNode}
end
StyleSheet() = StyleSheet(Set{CSSNode}())

Base.show(io::IO, m::MIME"text/html", n::StyleSheet) = begin
  write(io, "<style>")
  for s in n.styles
    show(io, "text/css", s)
  end
  write(io, "</style>")
  nothing
end

Base.show(io::IO, m::MIME"application/json", n::StyleSheet) = begin
  write(io, """{"tag":"style","children":[{"type":"Text","value":""")
  css = reduce((out,n)->out * sprint(show, MIME("text/css"), n), "", n.styles)
  show(io, m, css)
  write(io, "}]}")
  nothing
end

const global_sheet = StyleSheet()

macro css_str(string)
  node = parse_css(IOBuffer(string))
  push!(global_sheet.styles, node)
  hash(node) |> hex
end
