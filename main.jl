@use "github.com" [
  "jkroso/Prospects.jl" group mapcat assoc append @struct
  "jkroso/Promises.jl" @defer need
  "jkroso/DynamicVar.jl" @dynamic!
  "jkroso/Sequences.jl/collections/Map.jl" Map push
  "jkroso/Sequences.jl/collections/Set.jl" Setlet
  "jkroso/JSON.jl/write.jl"]
@use "./css" parse_css CSSNode class_name
@use MacroTools: MacroTools, @capture, @match
import Base.Iterators: filter

const runtime = joinpath(@dirname(), "runtime.js")
const empty_dict = Map{Symbol,Any}()
const empty_set = Setlet{Symbol}()

"Anything that implements diff, show, and emit"
abstract type Node end
"A simple UI node that can be converted to a HTML node"
abstract type Primitive <: Node end
@struct Container{tag}(attrs::AbstractDict{Symbol,Any}, children::AbstractVector{Node}) <: Primitive
@struct Text(value::AbstractString) <: Primitive
@struct Literal(value::AbstractString) <: Primitive

abstract type Patch end
@struct Replace(node::Node) <: Patch
@struct UpdateText(value::AbstractString) <: Patch
@struct Mutation(attrs::Vector{Patch}, children::Vector{Patch}) <: Patch
@struct RemoveAttribute(attr::Symbol) <: Patch
@struct UpdateClassList(remove::Set, add::Set) <: Patch
@struct SetAttribute(key::Symbol, value::Any) <: Patch
@struct UpdateStyle(remove::Vector{Symbol}, add::Vector{Pair{Symbol,Any}}) <: Patch
@struct Skip(n::Integer) <: Patch
@struct TrimChildren(n::Integer) <: Patch
@struct AppendChildren(nodes::Vector{Node}) <: Patch

Base.isempty(m::Mutation) = isempty(m.attrs) && isempty(m.children)

"Generate a Patch that can transform one Node into another"
function diff end

# handle a::Text,b::Container and vice versa
diff(a::Primitive, b::Primitive) = Replace(b)

diff(a::Text, b::Text) = a.value == b.value ? nothing : UpdateText(b.value)
diff(a::Node, b::Node) = Replace(b)
diff(a::Container{tag}, b::Container{tag}) where tag = begin
  a === b && return nothing
  m = Mutation(diff_attributes(a.attrs, b.attrs),
               diff_children(a.children, b.children))
  isempty(m) ? nothing : m
end

diff_attributes(a::AbstractDict, b::AbstractDict) = begin
  patches = Vector{Patch}()

  for key in keys(a)
    haskey(b, key) || push!(patches, RemoveAttribute(key))
  end

  for (key, value) in b
    isa(value, Function) && continue
    if key == :class
      diff = diff_class(get(a, :class, empty_set), value)
      isempty(diff) || push!(patches, diff)
    elseif key == :style
      diff = diff_style(get(a, :style, empty_dict), value)
      isempty(diff) || push!(patches, diff)
    elseif get(a, key, nothing) != value
      jsonable(value) && push!(patches, SetAttribute(key, value))
    end
  end

  patches
end

"A predicate to determine if an attribute should be sent to the runtime"
jsonable(value) = true
jsonable(::Function) = false

Base.isempty(u::UpdateClassList) = isempty(u.remove) && isempty(u.add)
Base.isempty(u::UpdateStyle) = isempty(u.remove) && isempty(u.add)

diff_class(a::AbstractSet, b::AbstractSet) = UpdateClassList(setdiff(a, b), setdiff(b, a))

diff_style(a::AbstractDict, b::AbstractDict) =
  UpdateStyle(collect(Symbol, filter(k -> !haskey(b, k), keys(a))),
              collect(Pair{Symbol,Any}, filter(p -> get(a, p[1], nothing) != p[2], b)))

diff_children(a::Vector{Node}, b::Vector{Node}) = begin
  patches = Vector{Patch}()

  skip = 0
  # mutate existing nodes
  for (ac, bc) ∈ zip(a, b)
    patch = diff(ac, bc)
    if patch == nothing
      skip += 1
    else
      if skip > 0
        push!(patches, Skip(skip))
        skip = 0
      end
      push!(patches, patch)
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

Base.show(io::IO, m::MIME"application/json", p::P) where P<:Patch = begin
  write(io, b"{\"command\":\"", P.name.name, '"')
  for f in fieldnames(P)
    write(io, b",\"", f, b"\":")
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

Base.show(io::IO, m::MIME"application/json", n::Container{tag}) where tag = begin
  write(io, b"{\"tag\":\"", tag, '"')
  attrs = n.attrs
  write(io, b",\"attrs\":{")
  first = true
  for (key, value) in attrs
    jsonable(value) || continue
    if first
      first = false
    else
      write(io, ',')
    end
    write(io, '"', key, b"\":")
    show(io, m, value)
  end
  write(io, '}')
  children = n.children
  if !isempty(children)
    write(io, b",\"children\":")
    show(io, m, children)
  end
  write(io, '}')
  nothing
end

"""
Syntax sugar for creating DOM trees

```julia
@dom [:div class=:selected] # Container{:div}(Attrs(:class=>:selected), [])
@dom [:div class.selected=true] # Container{:div}(Attrs(:class=>:selected), [])
@dom [:div{class, attrs...} children...] # Container{:div}(Attrs(:class=>class, atrrs...), [children...])
@dom [:div [:span "a"]] # Container{:div}(Attrs(), [Container{:span}(Attrs(), [Text("a")])])
```
"""
macro dom(node) transform(node) end

transform(node::Any) = esc(node)
transform(node::AbstractString) = Text(node)
transform(node::Expr) = begin
  if node.head in [:vect :hcat :vcat]
    args = mapcat(n->Meta.isexpr(n, :row) ? n.args : [n], node.args) # ignore rows
    tag, rest = (args[1], args[2:end])
    @capture(normalize_tag(tag), fn_(attrs__))
    extra_attrs, children = group(isattr, map(css_attr, rest))
    attrs = map(normalize_attr, [map(css_attr, attrs)..., extra_attrs...])
    attrs = map(wrap_event_handler, attrs)
    expr = :($fn(Attrs($(attrs...)), Node[$(map(transform, children)...)]))
    any(isfocus_attr, attrs) ? :(focus($expr)) : expr
  else
    esc(node) # some sort of expression that generates a child node
  end
end

"Designed to be specialized"
focus(node) = node
isfocus_attr(a) = Meta.isexpr(a, :call, 3) && a.args[1] == :(=>) && a.args[2] == QuoteNode(:focus)

wrap_event_handler(attr) = begin
  (Meta.isexpr(attr, :call, 3) && attr.args[1] == :(=>)) || return attr
  _, name, val = attr.args
  startswith(string(name), ":on") || return attr
  :($name => wrap_handler($name, $val))
end

"Designed to be specialized"
wrap_handler(name, fn) = fn

css_attr(x) = @capture(x, @css_str(_String)) ? :(:class => $x) : x
isattr(e) = @capture(e, (_ = _) | (_ => _))
toclass(s::String) = QuoteNode(Symbol(strip(s)))
toclass(s) = :(Symbol($(esc(s))))
normalize_attr(e) =
  @match e begin
    (class=s_string) => :(:class => Setlet(tuple($(map(toclass, s.args)...))))
    (class=s_String) => :(:class => $(Setlet((Symbol(x) for x in split(s)))))
    (a_.b_ = c_) => :($(QuoteNode(a)) => $(QuoteNode(b)) => $(esc(c)))
    ((:a_|a_) = b_) => :($(QuoteNode(a)) => $(esc(b)))
    (s_Symbol) => :($(QuoteNode(s)) => $(esc(s)))
    _ => esc(e)
  end
normalize_tag(tag) =
  @match tag begin
    :tag_{attrs__} => :(Container{$(QuoteNode(tag))}($(attrs...)))
    tag_{attrs__} => :($(esc(tag))($(attrs...)))
    :tag_ => :(Container{$(QuoteNode(tag))}())
    tag_ => :($(esc(tag))())
    _ => error("unknown tag pattern $tag")
  end

Attrs(attrs::Pair...) = reduce(add_attr, attrs, init=empty_dict)

add_attr(d::Map, (key,value)::Pair) = begin
  if key == :class
    add_class(d, value)
  elseif value isa Pair
    push(get(d, key, empty_dict), value)
  else
    push(d, key=>value)
  end
end

add_class(d::Map, class::Nothing) = d
add_class(d::Map, (name,bool)::Pair) = bool ? add_class(d, name) : d
add_class(d::Map, class::AbstractString) = add_class(d, split(class))
add_class(d::Map, classes::Union{AbstractSet,AbstractArray}) = begin
  if haskey(d, :class)
    assoc(d, :class, reduce(push, (Symbol(x) for x in classes), init=d[:class]))
  else
    push(d, :class=>Setlet{Symbol}(Symbol(x) for x in classes))
  end
end
add_class(d::Map, class::Symbol) = begin
  if haskey(d, :class)
    assoc(d, :class, push(d[:class], class))
  else
    push(d, :class=>Setlet{Symbol}((class,)))
  end
end

"Create a new node extended with an extra attribute"
add_attr(c::Container, key::Symbol, value::Any) = assoc(c, :attrs, add_attr(c.attrs, key, value))
add_attr(d::Map, key::Symbol, value::Any) = begin
  if key == :class
    add_class(d, value)
  elseif value isa Pair
    assoc(d, key, push(get(d, key, empty_dict), value))
  else
    assoc(d, key, value)
  end
end

Base.convert(::Type{Node}, a::AbstractString) = Text(a)
Base.convert(::Type{Node}, n::Union{Number,Symbol,Char}) = Text(string(n))
const null_node = @dom[:div style.position="absolute"]
Base.convert(::Type{Node}, ::Nothing) = null_node

const html_reserved = r"[\"'&<>]"
replace_char(m::AbstractString) = replace_char(m[1])
replace_char(m::Char) =
  if     m == '"' "&quot;"
  elseif m == '&' "&amp;"
  elseif m == '\'' "&#39;"
  elseif m == '<' "&lt;"
  elseif m == '>' "&gt;" end

escapeHTML(s::AbstractString) = replace(s, html_reserved=>replace_char)

write_style(io::IO, style::AbstractDict) =
  for (key,value) in style
    write(io, hyphencase(key), ':', string(value), ';')
  end

hyphencase(str::AbstractString) = replace(str, r"([a-z])([A-Z])" => m -> string(m[1], '-', lowercase(m[2])))
hyphencase(str) = hyphencase(string(str))

const self_closing = Set([:area, :base, :br, :col, :command, :embed, :hr,
                          :img, :input, :keygen, :link, :meta, :param,
                          :source, :track, :wbr])

Base.show(io::IO, m::MIME"text/html", t::Literal) = write(io, t.value)
Base.show(io::IO, m::MIME"text/html", t::Text) = write(io, escapeHTML(t.value))
Base.show(io::IO, m::MIME"text/html", n::Container{:style}) = write(io, "<style>", map(x->x.value, n.children)..., "</style>")
Base.show(io::IO, m::MIME"text/html", n::Container{:script}) = write(io, "<script>", map(x->x.value, n.children)..., "</script>")
Base.show(io::IO, m::MIME"text/html", n::Container{tag}) where tag = begin
  write(io, '<', tag)
  for (key, value) in n.attrs
    value isa Function && continue
    if key == :class
      value = join(value, ' ')
    elseif key == :style
      value = sprint(write_style, value)
    end
    if value isa Bool
      value && write(io, ' ', key)
    else
      write(io, ' ', key, "=\"", escapeHTML(value), '"')
    end
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

const styles = Set{CSSNode}()
const css = Ref(@defer @dom[:style]::Container{:style})

"""
Add some css to the global stylesheet and return a className so this css
can be applied to several DOM nodes. The `@dom` macro also has a special
case for this macro which allows you to write `@dom [:div css"color: red"]`
instead of `@dom [:div class=css"color: red"]`
"""
macro css_str(str)
  node = parse_css(str)
  push!(styles, node)
  css[] = @defer @dom[:style sprint(showstyles)]::Container{:style}
  QuoteNode(Symbol(class_name(node)))
end

showstyles(io) =
  for s in styles
    show(io, "text/css", s)
  end

"A CSS reset based on https://github.com/jgthms/minireset.css"
const reset = @dom[:style "html,body,p,ol,ul,li,dl,dt,dd,blockquote,figure,fieldset,legend,textarea,pre,iframe,hr,h1,h2,h3,h4,h5,h6{margin:0;padding:0}body{height:100%;width:100%;position:absolute}h1,h2,h3,h4,h5,h6{font-size:100%;font-weight:normal}ul{list-style:none}button,input,select,textarea{margin:0}html{box-sizing:border-box}*{box-sizing:inherit}*:before,*:after{box-sizing:inherit}img,embed,object,audio,video{height:auto;max-width:100%}iframe{border:0}table{border-collapse:collapse;border-spacing:0}td,th{padding:0;text-align:left}"]

"Defines an HTML document with the default stylesheets included"
HTML(attrs, children) =
  @dom[:html
    [:head [:meta charset="utf-8"] reset need(css[])]
    [:body{attrs...} children...]]

export @dom, @css_str, emit, diff, HTML
