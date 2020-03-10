@use "github.com/jkroso/AsyncBuffer.jl" pipe Buffer AsyncBuffer
@use "github.com/jkroso/Sequences.jl" Cons EOS
@use "." Text Container Node Attrs

const attr_regex = r"(\w+)(?:=(?:\"([^\"]*)\"|([^\s])*))?(?:\s|$|/)"
const scoped_property = r"([\w-]+)=\"?([^\"]+)\"?"
const empty_elements = ["area", "base", "br", "col", "embed", "hr", "img", "input", "link", "meta", "param", "source", "track", "wbr"]

const codes = Dict{String, Char}("&lt;" => '<', "&gt;" => '>', "&quot;" => '"', "&apos;" => '\'', "&amp;" => '&')
const decoders = [
  r"&#\d+;" => (d)->Char(parse(Int, d[3:end-1])),
  r"&x\d+;" => (d)->Char(parse(Int, d[3:end-1], base=16)),
  r"&\w{2,4};" => (d)->get(codes, d, d)
]
"decode HTML escape codes"
decode(s) = foldl(replace, decoders, init=s)

parseHTML(io::IO, stack) = begin
  txt = readuntil(io, '<', keep=false)
  if !isempty(txt)
    skip(io, -1)
    return Text(decode(txt))
  end
  tag, nxt = readtag(io)
  # closing tag
  tag == "" && return readuntil(io, '>', keep=false)
  # comment
  if startswith(tag, "!--")
    readuntil(io, "-->")
    return parseHTML(io, stack)
  end
  if occursin(r"!doctype"i, tag)
    readuntil(io, '>', keep=false)
    return skiptext_parseHTML(io, stack)
  end
  meta = nxt == '>' ? "" : readuntil(io, '>', keep=false)
  attrs = Attrs(map(eachmatch(attr_regex, meta)) do m
    key, v1, v2 = m.captures
    value = if v1 != nothing
      v1
    elseif v2 != nothing
      v2
    else
      true
    end
    if key == "style"
      value = parse_style(value)
    elseif key == "class"
      value = Set(map(Symbol, split(value, ' ')))
    end
    Symbol(key) => value
  end...)
  if occursin(scoped_property, tag)
    tag, value = match(scoped_property, tag).captures
    attrs[Symbol(string(tag))] = value
  end
  node = Container{Symbol(tag)}(attrs, [])
  if tag in empty_elements || nxt == '/' || endswith(meta, '/')
    node
  else
    parse_children(node, io, Cons(node, stack))
  end
end

parse_children(c::Container{:style}, io, stack) = (push!(c.children, readtext(io, "style")); c)
parse_children(c::Container{:script}, io, stack) = (push!(c.children, readtext(io, "script")); c)
parse_children(c::Container, io, stack) = begin
  while true
    eof(io) && break
    node = parseHTML(io, stack)
    # closing tag
    if node isa AbstractString
      node == tagname(c) && return c
      for parent in stack
        tagname(parent) == node && return node
      end
      error("unexpected closing tag: $node")
    end
    push!(c.children, node)
  end
  c
end

tagname(::Container{tag}) where tag = String(tag)

parse_style(s) =
  reduce(split(s, ';', keepempty=false), init=Dict{Symbol,Any}()) do dict, kv
    kv = strip(kv)
    isempty(kv) && return dict
    key, value = map(strip, split(kv, ':'))
    key = replace(key, r"[a-z]-[a-z]"=> s->string(s[1],uppercase(s[3])))
    dict[Symbol(key)] = value
    dict
  end

readtag(io::IO) = begin
  buf = IOBuffer()
  local c
  while true
    c = read(io, Char)
    if c == '!' # handle comments
      read(io, 2) == [0x2d, 0x2d] && return ("!--", read(io, Char))
      skip(io, -2)
    end
    (c == ' ' || c == '>' || c == '/') && break
    write(buf, c)
  end
  String(take!(buf)), c
end

readtext(io::IO, closing_tag::AbstractString) = Text(readuntil(io, "</$closing_tag>", keep=false))
skiptext_parseHTML(io::IO, stack) = begin
  while true
    html = parseHTML(io, stack)
    html isa Text && continue
    return html
  end
end

goodIO(io::IO) = pipe(io, Buffer())
goodIO(io::Union{IOBuffer,AsyncBuffer}) = io
goodIO(x::Any) = IOBuffer(x)
Base.parse(::MIME"text/html", data::Any) = parseHTML(goodIO(data), EOS)::Node

macro html_str(str)
  parse(MIME("text/html"), str)
end
