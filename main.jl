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
      # check the value can actually be encoded
      if method_exists(show, Tuple{IO,MIME"application/json",typeof(value)})
        push!(patches, SetAttribute(key, value))
      end
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
      value = filter((k,v)->applicable(show, io, m, v), value)
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
    fn = istag(tag) ? :(Container{$tag}) : :($(esc(tag)))
    rest = map(css_attr, rest)
    if findfirst(isdynamic, rest) > 0
      rest = map(x->isattr(x) ? topair(x) : transform(x), rest)
      :($fn(merge_attrs_children($(rest...))...))
    else
      attrs, children = group(isattr, rest)
      attrs = :(merge_attrs($(map(topair, attrs)...)))
      children = :(Node[$(map(transform, children)...)])
      :($fn($attrs, $children))
    end
  else
    esc(node)
  end
end

css_attr(x::Any) = x
css_attr(x::Expr) = x.head ≡ :macrocall && x.args[1] ≡ Symbol("@css_str") ?
  :($(QuoteNode(:class)) => $x) :
  x
istag(::Any) = false
istag(x::Expr) = x.head ≡ :quote && isa(x.args[1], Symbol)
istag(x::QuoteNode) = isa(tag.value, Symbol)
isdynamic(::Any) = false
isdynamic(::Symbol) = true
isdynamic(e::Expr) = !(isattr(e) || e.head in [:vect :hcat :vcat]) || e.head ≡ :...
isattr(::Any) = false
isattr(e::Expr) = e.head ≡ :(=) || e.head ≡ :(=>)
topair(e::Expr) = begin
  isa(e, Expr) && e.head ≡ :(=>) && return e
  a,b = e.args
  if isa(a, Expr) && a.head ≡ :quote # handle :type="radio" etc..
    :($a => $(esc(b)))
  elseif isa(a, Symbol)
    :($(QuoteNode(a)) => $(esc(b)))
  elseif isa(a, Expr) && a.head ≡ :.
    :($(QuoteNode(a.args[1])) => $(a.args[2]) => $(esc(b)))
  else
    error("unknown attribute format $e")
  end
end

merge_attrs(attrs::Pair...) = reduce(add_attr, Base.ImmutableDict{Symbol,Any}(), attrs)

add_attr(d::Base.ImmutableDict, p::Pair) = begin
  key,value = p
  if key ≡ :class
    add_class(d, value)
  else
    assoc(d, key, value)
  end
end

add_class(d::Base.ImmutableDict, class::Pair) = class[2] ? add_class(d, class[1]) : d
add_class(d::Base.ImmutableDict, class::AbstractString) = add_class(d, Symbol(class))
add_class(d::Base.ImmutableDict, class::Union{Set,AbstractArray}) =
  if haskey(d, :class)
    union!(d[:class], class); d
  else
    assoc(d, :class, Set{Symbol}(class))
  end
add_class(d::Base.ImmutableDict, class::Symbol) =
  if haskey(d, :class)
    push!(d[:class], class); d
  else
    assoc(d, :class, Set{Symbol}([class]))
  end

merge_attrs_children(items...) = begin
  attrs = Base.ImmutableDict{Symbol,Any}()
  children = Node[]
  for item in items
    if isa(item, Pair)
      attrs = add_attr(attrs, item)
    else
      push!(children, item)
    end
  end
  (attrs, children)
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
  node = parse_css(string)
  push!(global_sheet.children[1].styles, node)
  QuoteNode(Symbol('_', hash(node) |> hex))
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

export @dom, @css_str, stylesheets, Node, Container, Text
