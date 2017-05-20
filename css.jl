@require "github.com/jkroso/Prospects.jl" exports...

@struct CSSNode(attrs=Dict{Symbol,Any}(), children=Dict{Vector{String},CSSNode}())

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
    if ismatch(inline_block, line)
      selector, attrs = match(inline_block, line).captures
      selectors = map(strip, split(selector, ','))
      child = CSSNode()
      stack[end].children[selectors] = child
      for m in eachmatch(attr_regex, strip(attrs))
        child.attrs[Symbol(m[1])] = m[2]
      end
    elseif ismatch(attr_regex, line)
      for m in eachmatch(attr_regex, strip(line))
        stack[end].attrs[Symbol(m[1])] = m[2]
      end
    else
      selectors = map(strip, split(line, ','))
      child = stack[end].children[selectors] = CSSNode()
      push!(stack, child)
    end
  end
  stack[1]
end

const attr_regex = r"(\w[^:]+):\s+(url\([^)]*\)|[^;]+)"
const inline_block = r"([^{]+){([^}]+)}"

macro css_str(str) parse_css(str) end
