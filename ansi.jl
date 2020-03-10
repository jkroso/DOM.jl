@require "." @dom

"Handle ANSI color sequences"
ansi(str) = begin
  matches = collect(eachmatch(r"\e\[(\d{2})m", str))
  isempty(matches) && return @dom[:span str]
  out = [@dom[:span style.color="lightgray" str[1:matches[1].offset-1]]]
  for (i, current) in enumerate(matches)
    start = current.offset + length(current.match)
    cutoff = i == lastindex(matches) ? lastindex(str) : matches[i+1].offset-1
    color = colors[parse(UInt8, current.captures[1]) - UInt8(30)]
    text = str[start:cutoff]
    push!(out, @dom[:span{style.color=color} text])
  end
  @dom[:p out...]
end

const colors = Dict{UInt8,String}(
  0 => "black",
  1 => "red",
  2 => "green",
  3 => "yellow",
  4 => "blue",
  5 => "magenta",
  6 => "cyan",
  7 => "white",
  9 => "lightgray",
  60 => "lightblack",
  61 => "#f96666",
  62 => "lightgreen",
  63 => "lightyellow",
  64 => "lightblue",
  65 => "lightmagenta",
  66 => "lightcyan",
  67 => "lightwhite")
