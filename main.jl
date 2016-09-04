@require "github.com/jkroso/write-json.jl"

const runtime = joinpath(@dirname(), "runtime.js")

abstract Node
abstract Primitive <: Node

immutable Container{tag} <: Primitive
  attrs::Dict{Symbol,Any}
  children::Vector{Node}
end

immutable Text <: Primitive
  value::AbstractString
end

abstract Patch

immutable Replace <: Patch
  node::Node
end

immutable UpdateText <: Patch
  value::AbstractString
end

immutable Mutation <: Patch
  attrs::Vector{Patch}
  children::Vector{Patch}
end

Base.isempty(m::Mutation) = isempty(m.attrs) && isempty(m.children)

# handle a::Text,b::Container and vice versa
diff(a::Primitive, b::Primitive) = Nullable{Patch}(Replace(b))

diff(a::Text, b::Text) = Nullable{Patch}(a.value == b.value ? nothing : UpdateText(b.value))
diff(a::Container, b::Container) = Nullable{Patch}(Replace(b))
diff{tag}(a::Container{tag}, b::Container{tag}) = begin
  is(a, b) && return Nullable{Patch}()
  m = Mutation(diff_attributes(a.attrs, b.attrs),
               diff_children(a.children, b.children))
  Nullable{Patch}(isempty(m) ? nothing : m)
end

immutable RemoveAttribute <: Patch
  attr::Symbol
end

diff_attributes(a::Dict, b::Dict) = begin
  patches = Vector{Patch}()

  for key in keys(a)
    haskey(b, key) || push!(patches, RemoveAttribute(key))
  end

  for (key, value) in b
    if key == :class
      diff = diff_class(get(a, :class, Set()), value)
      isempty(diff) || push!(patches, diff)
    elseif key == :style
      diff = diff_style(get(a, :style, Dict()), value)
      isempty(diff) || push!(patches, diff)
    elseif get(a, key, nothing) != value
      push!(patches, SetAttribute(key, value))
    end
  end

  patches
end

diff_class(a::Set, b::Set) = UpdateClassList(setdiff(a, b), setdiff(b, a))

immutable UpdateClassList <: Patch
  remove::Set
  add::Set
end

Base.isempty(u::UpdateClassList) = isempty(u.remove) && isempty(u.add)

immutable UpdateStyle <: Patch
  remove::Vector{Symbol}
  add::Vector{Pair{Symbol,Any}}
end

Base.isempty(u::UpdateStyle) = isempty(u.remove) && isempty(u.add)

diff_style(a::Dict, b::Dict) =
  UpdateStyle(filter(k -> haskey(b, k), keys(a)),
              map(k-> k => b[k], filter(k -> get(a, k, nothing) == b[k], keys(b))))

immutable SetAttribute <: Patch
  key::Symbol
  value::Any
end

immutable Skip <: Patch
  n::Integer
end

diff_children(a::Vector{Node}, b::Vector{Node}) = begin
  patches = Vector{Patch}()

  skip = 0
  # mutate existing nodes
  for i in 1:min(length(a), length(b))
    patch = diff(a[i], b[i])
    if isnull(patch)
      skip += 1
    else
      if skip > 0
        push!(patches, Skip(skip))
        skip = 0
      end
      push!(patches, get(patch))
    end
  end

  # remove extra nodes
  if length(a) > length(b)
    push!(patches, TrimChildren(length(a) - length(b)))
  # add new nodes
  elseif length(b) > length(a)
    push!(patches, AppendChildren(b[length(a):end]))
  end

  patches
end

immutable TrimChildren <: Patch
  n::Integer
end

immutable AppendChildren <: Patch
  nodes::Vector{Node}
end

Base.show{P<:Patch}(io::IO, m::MIME"application/json", p::P) = begin
  write(io, "{\"command\":\"$(P.name.name)\"")
  for f in fieldnames(P)
    write(io, ",\"$f\":")
    show(io, m, getfield(p, f))
  end
  write(io, '}')
end

Base.show{N<:Node}(io::IO, m::MIME"application/json", n::N) = begin
  write(io, "{\"type\":\"$(N.name.name)\"")
  if N <: Container
    write(io, ",\"tag\":\"$(N.parameters[1])\"")
  end
  for f in fieldnames(N)
    write(io, ",\"$f\":")
    show(io, m, getfield(n, f))
  end
  write(io, '}')
end

Base.:(==){tag}(a::Container{tag}, b::Container{tag}) = a.attrs == b.attrs && a.children == b.children
Base.:(==)(a::Text, b::Text) = a.value == b.value
Base.:(==){P<:Patch}(a::P, b::P) = begin
  for f in fieldnames(P)
    getfield(a, f) == getfield(b, f) || return false
  end
  true
end


macro dom(node) transform(node) end

transform(node::Any) = esc(node)
transform(node::AbstractString) = Text(node)
transform(node::Expr) = begin
  if node.head == :vect || node.head == :hcat || node.head == :vcat
    tag = node.args[1]
    attrs = :(Dict($(map(topair, filter(isattr, node.args[2:end]))...)))
    children = :([$(map(transform, filter(a->!isattr(a), node.args[2:end]))...)])
    :(Container{$tag}($attrs, $children))
  else
    esc(node)
  end
end

isattr(::Any) = false
isattr(e::Expr) = e.head == :(=)
topair(e::Expr) = :($(QuoteNode(e.args[1])) => $(e.args[2]))

Base.convert(::Type{Node}, a::AbstractString) = Text(a)
Base.convert(::Type{Node}, n::Number) = Text(string(n))

const html_reserved = r"[\"'&<>]"
replace_char(m::AbstractString) = replace_char(m[1])
replace_char(m::Char) =
  if     m == '"' "&quot;"
  elseif m == '&' "&amp;"
  elseif m == '\'' "&#39;"
  elseif m == '<' "&lt;"
  elseif m == '>' "&gt;" end

escapeHTML(s::AbstractString) = replace(s, html_reserved, replace_char)

write_style(io::IO, style::Dict) =
  for (key,value) in style
    write(io, key, ':', value, ';')
  end

const self_closing = Set([:area, :base, :br, :col, :command, :embed, :hr,
                          :img, :input, :keygen, :link, :meta, :param,
                          :source, :track, :wbr])

Base.show(io::IO, m::MIME"text/html", t::Text) = write(io, t.value)

Base.show{tag}(io::IO, m::MIME"text/html", n::Container{tag}) = begin
  write(io, '<', tag)
  for (key, value) in n.attrs
    if key == :class
      value = join(value, ' ')
    elseif key == :style
      value = sprint(write_style, value)
    end
    write(io, ' ', key, "=\"", escapeHTML(value), '"')
  end
  if tag in self_closing
    write(io, "/>")
  else
    write(io, '>')
    for child in n.children
      show(io, m, child)
    end
    write(io, "</", tag, '>')
  end
end
