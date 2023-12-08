@use MathJaxRenderer: Math, svg_converter
@use LaTeXStrings: @L_str, LaTeXString
@use "./html.jl"
@use "." Node

Base.convert(::Type{Node}, s::LaTeXString) = begin
  tex = replace(String(s), r"^\$+"=>"", r"([^\\%]\$)+$"=>"")
  math = Math(tex)
  svg = String(take!(svg_converter(math)))
  parse(MIME("text/html"), svg)
end
