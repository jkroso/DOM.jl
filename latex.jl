@use "github.com/stevengj/LaTeXStrings.jl" @L_str LaTeXString
@use "github.com/MichaelHatherly/MathJaxRenderer.jl" Math svg_converter
@use "./html.jl"
@use "." Node

Base.convert(::Type{Node}, s::LaTeXString) = begin
  math = Math(String(s)[2:end-1])
  svg = String(take!(svg_converter(math)))
  parse(MIME("text/html"), svg)
end
