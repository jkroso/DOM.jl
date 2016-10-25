@require "github.com/jkroso/Prospects.jl" group

immutable CSSNode
  attrs::Dict{Symbol,Any}
  children::Dict{String,CSSNode}
end

CSSNode() = CSSNode(Dict{Symbol,Any}(), Dict{String,CSSNode}())

# hash needs to be stable and hash(Dict) normally isn't stable
hashpair(h::UInt, p::Pair) = hash(p, h)
Base.hash(s::CSSNode, h::UInt) = reduce(hashpair, reduce(hashpair, h, s.children), s.attrs)
Base.:(==)(a::CSSNode, b::CSSNode) = a.attrs == b.attrs && a.children == b.children

macro css(node) transform(node) end

transform(n::Expr) = begin
  @assert isvec(n)
  attrs, children = group(isattr, n.args)
  :(CSSNode(Dict($(map(topair, attrs)...)),
          Dict($(map(tostyle, children)...))))
end

isvec(e::Any) = false
isvec(e::Expr) = e.head in [:vect, :hcat, :vcat]
isattr(e::Any) = false
isattr(e::Expr) = e.head == Symbol(':') && !isvec(e.args[2])

tostyle(e::Expr) = :($(to_selector(e.args[1])) => $(transform(e.args[2])))
to_selector(s::AbstractString) = string(s)
to_selector(s::Symbol) = string(s)
topair(e::Expr) = :($(handle_kebab_case(e.args[1])) => $(e.args[2]))

handle_kebab_case(s::AbstractString) = s
handle_kebab_case(e::Symbol) = string(e)
handle_kebab_case(e::Expr) = e.head == :call && e.args[1] == :- ?
  join(e.args[2:end], '-') :
  e

Base.show(io::IO, ::MIME"text/css", s::CSSNode) = write_node(io, s, "." * hex(hash(s)))

write_node(io::IO, n::CSSNode, selector::String) = begin
  if !isempty(n.attrs)
    write(io, selector, '{')
    write_attrs(io, n.attrs)
    write(io, '}')
  end
  for (path, node) in n.children
    write_node(io, node, join([selector,path], ' '))
  end
end

write_attrs(io::IO, d::Dict) = begin
  for (key, value) in d
    write(io, key, ':', value, ';')
  end
end

Base.convert(::Type{CSSNode}, pairs::Union{Vector,Dict}) = begin
  attrs = Dict{Symbol,Any}()
  children = Dict{String,CSSNode}()
  for (key, value) in pairs
    if isa(value, Vector) || isa(value, Dict)
      children[key] = convert(CSSNode, value)
    else
      attrs[key] = value
    end
  end
  CSSNode(attrs, children)
end

macro css_str(string) parse_css(IOBuffer(string)) end

parse_css(buf) = begin
  stack = [CSSNode()]
  for line in eachline(buf)
    expected_indent = 2(length(stack) - 1)
    indent, = Base.indentation(line)
    if indent < expected_indent pop!(stack) end
    key,value = map(strip, split(line, ':'))
    if isempty(value)
      child = stack[end].children[key] = CSSNode()
      push!(stack, child)
    else
      stack[end].attrs[Symbol(key)] = value
    end
  end
  stack[1]
end
