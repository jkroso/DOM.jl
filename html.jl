@require "github.com/jkroso/AsyncBuffer.jl" pipe Buffer AsyncBuffer
@require "." Text Container Node Attrs

const attr_regex = r"(\w+)(?:=(?:\"([^\"]*)\"|([^\s])*))?(?:\s|$|/)"

parseHTML(io::IO) = begin
  txt = readuntil(io, '<', keep=false)
  if !isempty(txt)
    skip(io, -1)
    return Text(txt)
  end
  tag, nxt = readtag(io)
  # closing tag
  tag == "" && return readuntil(io, '>', keep=false)
  # comment
  if tag == "!--"
    readuntil(io, "-->")
    return parseHTML(io)
  end
  meta = nxt == '>' || nxt =='/' ? "" : readuntil(io, '>', keep=false)
  attrs = Attrs(map(eachmatch(attr_regex, meta)) do m
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
  children = nxt == '/' || endswith(meta, '/') ? [] : parseuntil(io, tag)
  Container{Symbol(tag)}(attrs, children)
end

readtag(io::IO) = begin
  buf = IOBuffer()
  local c
  while true
    c = read(io, Char)
    (c == ' ' || c == '>' || c == '/') && break
    write(buf, c)
  end
  String(take!(buf)), c
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
