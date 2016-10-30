@require "github.com/jkroso/Prospects.jl" group mapcat assoc
@require "github.com/jkroso/write-json.jl"
@require "./Events" => Events Event
@require "./css" parse_css CSSNode

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
    isa(value, Function) && continue
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
    push!(patches, AppendChildren(b[(length(a)+1):end]))
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
  nothing
end

Base.show(io::IO, m::MIME"application/json", n::Text) = begin
  write(io, b"{\"type\":\"Text\",\"value\":")
  show(io, m, n.value)
  write(io, '}')
  nothing
end

Base.show{tag}(io::IO, m::MIME"application/json", n::Container{tag}) = begin
  write(io, "{\"tag\":\"$tag\"")
  for field in [:attrs :children]
    value = getfield(n, field)
    if field ≡ :attrs
      value = filter((key,val)->!isa(val,Function), value)
    end
    isempty(value) && continue
    write(io, b",\"", field, b"\":")
    show(io, m, value)
  end
  write(io, '}')
  nothing
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
  if node.head in [:vect :hcat :vcat]
    args = mapcat(n->isa(n, Expr) && n.head ≡ :row ? n.args : [n], node.args) # ignore rows
    tag, rest = (args[1], args[2:end])
    attrs, children = group(isattr, rest)
    attrs = :(merge_attrs($(map(topair, attrs)...)))
    children = :(Node[$(map(transform, children)...)])
    if (isa(tag, Expr) && tag.head ≡ :quote && isa(tag.args[1], Symbol)) || (isa(tag, QuoteNode) && isa(tag.value, Symbol))
      :(Container{$tag}($attrs, $children))
    else
      :($(esc(tag))($attrs, $children))
    end
  else
    esc(node)
  end
end

isattr(::Any) = false
isattr(e::Expr) = e.head ≡ :(=)
topair(e::Expr) = begin
  a,b = e.args
  if isa(a, Expr) && a.head ≡ :quote # handle :type="radio" etc..
    :($a => $(esc(b)))
  elseif isa(a, Symbol)
    :($(QuoteNode(a)) => $(esc(b)))
  elseif isa(a, Expr) && a.head ≡ :call && a.args[1] ≡ :-
    :($(QuoteNode(a.args[2])) => $(QuoteNode(a.args[3])) => $(esc(b)))
  else
    error("unknown attribute format $e")
  end
end

merge_attrs(attrs::Pair...) = begin
  out = Base.ImmutableDict{Symbol,Any}()
  classes = Set{Symbol}()
  for (key,value) in attrs
    if key ≡ :class
      if isa(value, Pair)
        value[2] && push!(classes, value[1])
      elseif isa(value, AbstractString)
        push!(classes, Symbol(value))
      elseif isa(value, Symbol)
        push!(classes, value)
      elseif isa(value, Set)
        classes = union(classes, value)
      else
        append!(classes, value)
      end
    else
      out = assoc(out, key, value)
    end
  end
  isempty(classes) ? out : assoc(out, :class, classes)
end

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

const event_names = Dict(Events.KeyUp=>:onkeyup,
                         Events.KeyDown=>:onkeydown,
                         Events.MouseMove=>:onmousemove,
                         Events.MouseDown=>:onmousedown,
                         Events.MouseUp=>:onmouseup,
                         Events.Click=>:onclick,
                         Events.DoubleClick=>:ondblclick,
                         Events.MouseOut=>:onmouseout,
                         Events.MouseOver=>:onmouseover,
                         Events.Focus=>:onfocus,
                         Events.Blur=>:onblur)

"""
Find the DOM node the event targets and invoke its handling function
"""
dispatch(n::Container, e::Event) = begin
  for key in Events.path(e)
    n = n.children[key]
  end
  T = typeof(e)
  haskey(event_names, T) || return nothing
  get(n.attrs, event_names[T], identity)(e)
end

immutable StyleSheet <: Node
  styles::Set{CSSNode}
end

Base.show(io::IO, m::MIME"text/html", n::StyleSheet) =
  for s in n.styles
    show(io, "text/css", s)
  end

Base.show(io::IO, m::MIME"application/json", n::StyleSheet) = begin
  write(io, b"""{"type":"Text","value":""")
  show(io, m, sprint(show, MIME("text/html"), n))
  write(io, '}')
  nothing
end

diff(a::StyleSheet, b::StyleSheet) = begin
  atxt = sprint(show, MIME("text/html"), a)
  btxt = sprint(show, MIME("text/html"), b)
  Nullable{Patch}(atxt == btxt ? nothing : UpdateText(btxt))
end

macro css_str(string)
  node = parse_css(IOBuffer(string))
  push!(global_sheet.children[1].styles, node)
  "_" * (hash(node) |> hex)
end

"All CSS generated by the @css_str macro ends up in this stylesheet"
const global_sheet = Container{:style}(Dict(), [StyleSheet(Set{CSSNode}())])

"A CSS reset based on https://github.com/jgthms/minireset.css"
const reset = Container{:style}(Dict(), [Text("html,body,p,ol,ul,li,dl,dt,dd,blockquote,figure,fieldset,legend,textarea,pre,iframe,hr,h1,h2,h3,h4,h5,h6{margin:0;padding:0}body{height:100%;width:100%;position:absolute}h1,h2,h3,h4,h5,h6{font-size:100%;font-weight:normal}ul{list-style:none}button,input,select,textarea{margin:0}html{box-sizing:border-box}*{box-sizing:inherit}*:before,*:after{box-sizing:inherit}img,embed,object,audio,video{height:auto;max-width:100%}iframe{border:0}table{border-collapse:collapse;border-spacing:0}td,th{padding:0;text-align:left}")])

"""
Sheets which all projects should put in their document head

```julia
@dom [:html
  [:head sheets...]
  [:body]]
```
"""
const stylesheets = [global_sheet, reset]

export @dom, @css_str, stylesheets
