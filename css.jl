@require "github.com/jkroso/Prospects.jl" exports...

immutable CSSNode
  attrs::Dict{Symbol,Any}
  children::Dict{Vector{String},CSSNode}
end

CSSNode() = CSSNode(Dict{Symbol,Any}(), Dict{String,CSSNode}())

# hash needs to be stable and hash(Dict) normally isn't stable
hashpair(h::UInt, p::Pair) = hash(p, h)
Base.hash(s::CSSNode, h::UInt) = reduce(hashpair, reduce(hashpair, h, s.children), s.attrs)
Base.:(==)(a::CSSNode, b::CSSNode) = a.attrs == b.attrs && a.children == b.children

Base.show(io::IO, ::MIME"text/css", s::CSSNode) = write_node(io, s, ["._" * hex(hash(s))])

write_node(io::IO, n::CSSNode, selectors::Vector) = begin
  if !isempty(n.attrs)
    join(io, selectors, ',')
    write(io, '{')
    for (key, value) in n.attrs
      write(io, key, ':', value, ';')
    end
    write(io, '}')
  end
  for (paths, node) in n.children
    subselectors = mapcat(paths) do path
      if startswith(path, '&')
        map(s->s * path[2:end], selectors)
      else
        map(s->join([s, path], ' '), selectors)
      end
    end
    write_node(io, node, subselectors)
  end
end

parse_css(str::String) = begin
  str = Base.unindent(str, match(r"^\s*", str).match|>length)
  stack = [CSSNode()]
  for line in eachline(IOBuffer(str))
    expected_indent = 2(length(stack) - 1)
    indent = Base.indentation(line) |> first
    indent < expected_indent && pop!(stack)
    if ismatch(attr_regex, line)
      for match in eachmatch(attr_regex, strip(line))
        key,value = match.captures
        stack[end].attrs[Symbol(key)] = value
      end
    else
      selectors = map(strip, split(line, ','))
      child = stack[end].children[selectors] = CSSNode()
      push!(stack, child)
    end
  end
  stack[1]
end

attr_regex = r"(\w[^:]+):\s+(url\([^)]*\)|[^;]+)"

macro css_str(str) parse_css(str) end
