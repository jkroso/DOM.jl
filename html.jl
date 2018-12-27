@require "github.com/jkroso/AsyncBuffer.jl" pipe Buffer AsyncBuffer
@require "." Text Container Node Attrs

const attr_regex = r"(\w+)(?:=(?:\"([^\"]*)\"|([^\s])*))?(?:\s|$|/)"
const tag_regex = r"(\w+)(?:\s|$)"

parseHTML(io::IO) = begin
  txt = readuntil(io, '<', keep=false)
  if !isempty(txt)
    skip(io, -1)
    return Text(txt)
  end
  meta = readuntil(io, '>', keep=false)
  # closing tag
  meta[1] == '/' && return strip(meta[2:end])
  tag = match(tag_regex, meta).captures[1]
  attrs = Attrs(map(eachmatch(attr_regex, meta[length(tag)+1:end])) do m
    key, v1, v2 = m.captures
    value = if v1 != nothing
      v1
    elseif v2 != nothing
      v2
    else
      true
    end
    Symbol(key) => value
  end...)
  T = Container{Symbol(tag)}
  # self closing
  if meta[end] == '/'
    T(attrs, [])
  else
    T(attrs, parseuntil(io, tag))
  end
end

parseuntil(io::IO, closing_tag::AbstractString) = begin
  out = []
  while true
    eof(io) && break
    node = parseHTML(io)
    if node isa AbstractString
      @assert node == closing_tag "expected </$closing_tag> but got </$node>"
      break
    end
    push!(out, node)
  end
  out
end

goodIO(io::IO) = pipe(io, Buffer())
goodIO(io::Union{IOBuffer,AsyncBuffer}) = io
goodIO(x::Any) = IOBuffer(x)
Base.parse(::MIME"text/html", data::Any) = parseHTML(goodIO(data))::Node

macro html_str(str)
  parse(MIME("text/html"), str)
end
