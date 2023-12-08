@use "github.com/jkroso/Prospects.jl" exports...

@struct CSSNode(attrs=Dict{Symbol,Any}(),
                children=Dict{Vector{String},CSSNode}())

Base.show(io::IO, ::MIME"text/css", s::CSSNode) = write_node(io, s, String[selector(s)])
selector(s::CSSNode) = "." * class_name(s)
class_name(s::CSSNode) = class_name(string(hash(s), base=36))
class_name(s::String) = begin
  safe = lstrip(isnumeric, s)
  length(safe) < 4 ? "_" * s : safe
end

write_node(io::IO, n::CSSNode, selectors::Vector{String}) = begin
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
      if path == ":root"
        [":root"]
      elseif startswith(path, '&')
        map(s->s * path[2:end], selectors)
      else
        map(s->join([s, path], ' '), selectors)
      end
    end
    write_node(io, node, convert(Vector{String}, subselectors))
  end
end

parse_css(str::String) = begin
  str = Base.unindent(str, match(r"^\s*", str).match|>length)
  stack = [CSSNode()]
  for line in eachline(IOBuffer(str))
    expected_indent = 2(length(stack) - 1)
    indent = Base.indentation(line) |> first
    indent < expected_indent && pop!(stack)
    if occursin(inline_block, line)
      selector, attrs = match(inline_block, line).captures
      selectors = parse_selectors(selector)
      child = CSSNode()
      stack[end].children[selectors] = child
      for m in eachmatch(attr_regex, strip(attrs))
        child.attrs[Symbol(m[1])] = m[2]
      end
    elseif occursin(attr_regex, line)
      for m in eachmatch(attr_regex, strip(line))
        stack[end].attrs[Symbol(m[1])] = m[2]
      end
    else
      selectors = parse_selectors(line)
      child = stack[end].children[selectors] = CSSNode()
      push!(stack, child)
    end
  end
  stack[1]
end

const attr_regex = r"((?:-{1,2}|\w)[^:]+):\s+(url\([^)]*\)|[^;]+)"
const inline_block = r"([^{]+){([^}]+)}"

parse_selector(str) = begin
  selector = PipeBuffer()
  inbrackets = false
  for c in str
    if c == '('
      @assert inbrackets == false
      inbrackets = true
    elseif c == ')'
      @assert inbrackets == true
      inbrackets = false
    elseif c == ',' && !inbrackets
      break
    end
    write(selector, c)
  end
  String(take!(selector))
end

parse_selectors(str) = begin
  len = length(str)
  selectors = String[]
  i = 1
  while true
    selector = parse_selector(@view str[i:end])
    push!(selectors, selector)
    i += length(selector) + 1
    i >= len && break
  end
  map(strip, selectors)
end

macro css_str(str) parse_css(str) end
