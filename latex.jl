@require "github.com/stevengj/LaTeXStrings.jl" @L_str LaTeXString
@require "./html.jl"
@require "." Node

"""
Enables you to render LaTeXStrings. Needs the tex2svg command installed

```julia
@dom[:div L"\\mathrm{length}(\\textrm{city})"])
```
"""
Base.convert(::Type{Node}, s::LaTeXString) =
  open(cache(s), "r") do io
    parse(MIME("text/html"), io)
  end

cache(str) = begin
  s = String(str)[2:end-1]
  p = joinpath(tempdir(), string(hash(s)))
  if !ispath(p)
    run(pipeline(`tex2svg $s`, p))
  end
  p
end
