@require "github.com/JuliaCollections/OrderedCollections.jl" LittleDict
@require "github.com/MikeInnes/MacroTools.jl" => MacroTools @capture @match
@require "github.com/jkroso/Prospects.jl" group mapcat assoc push @struct
@require "github.com/jkroso/DynamicVar.jl" @dynamic!
@require "github.com/jkroso/write-json.jl"
@require "./Events" => Events Event
@require "./css" parse_css CSSNode
import Base.Iterators: filter

const runtime = joinpath(@dirname(), "runtime.js")
const empty_dict = Dict{Symbol,Any}()
const empty_ordered_dict = LittleDict{Symbol,Any}()
const empty_set = Set{Symbol}()

"Anything that implements diff, show, and emit"
abstract type Node end
"A simple UI node that can be converted to a HTML node"
abstract type Primitive <: Node end
@struct Container{tag}(attrs::AbstractDict{Symbol,Any}, children::AbstractVector{Node}) <: Primitive
@struct Text(value::AbstractString) <: Primitive

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
      # check the value can actually be encoded
      if hasmethod(show, Tuple{IO, MIME"application/json", typeof(value)})
        push!(patches, SetAttribute(key, value))
      end
    end
  end

  patches
end

Base.isempty(u::UpdateClassList) = isempty(u.remove) && isempty(u.add)
Base.isempty(u::UpdateStyle) = isempty(u.remove) && isempty(u.add)

diff_class(a::Set, b::Set) = UpdateClassList(setdiff(a, b), setdiff(b, a))

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
    value isa Function && continue
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
    :($fn(Attrs($(attrs...)), Node[$(map(transform, children)...)]))
  else
    esc(node) # some sort of expression that generates a child node
  end
end

css_attr(x) = @capture(x, @css_str(_String)) ? :(:class => $x) : x
isattr(e) = @capture(e, (_ = _) | (_ => _))
toclass(s::String) = QuoteNode(Symbol(strip(s)))
toclass(s) = :(Symbol($(esc(s))))
normalize_attr(e) =
  @match e begin
    (class=s_string) => :(:class => Set([$(map(toclass, s.args)...)]))
    (class=s_String) => :(:class => $(Set(map(Symbol, split(s, ' ')))))
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

Attrs(attrs::Pair...) = reduce(add_attr!, attrs, init=LittleDict{Symbol,Any}())

add_attr!(d::AbstractDict, (key,value)::Pair) = begin
  if key ≡ :class
    add_class!(d, value)
  elseif value isa Pair
    push!(get!(LittleDict{Symbol,Any}, d, key), value)
  else
    d[key] = value
  end
  d
end

add_class!(d::AbstractDict, class::Nothing) = d
add_class!(d::AbstractDict, (name,bool)::Pair) = bool ? add_class!(d, name) : d
add_class!(d::AbstractDict, class::AbstractString) = add_class!(d, Symbol(class))
add_class!(d::AbstractDict, class::Union{Set,AbstractArray}) =
  if haskey(d, :class)
    union!(d[:class], (Symbol(x) for x in class)); d
  else
    d[:class] = Set{Symbol}((Symbol(x) for x in class)); d
  end
add_class!(d::AbstractDict, class::Symbol) =
  if haskey(d, :class)
    push!(d[:class], class); d
  else
    d[:class] = Set{Symbol}([class]); d
  end

"Create a new node extended with an extra attribute"
add_attr(c::Container, key::Symbol, value::Any) = assoc(c, :attrs, add_attr(c.attrs, key, value))
add_attr(d::AbstractDict, key::Symbol, value::Any) = begin
  if key ≡ :class
    add_class!(assoc(d, :class, copy(get(d, :class, empty_set))), value)
  elseif value isa Pair
    assoc(d, key, push(get(d, key, empty_ordered_dict), value))
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

Base.show(io::IO, m::MIME"text/html", t::Text) = write(io, t.value)
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
      write(io, ' ', key)
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

"""
Invoke the handler of an event's target and then each of its parents
"""
propagate(n::Container, e) = propagate(Events.path(n, e), e)

Events.path(n::Container, e::Event) = begin
  path = Events.path(e)
  l = length(path) + 1
  nodes = Vector{Node}(undef, l)
  nodes[1] = n
  for i in 2:l
    n = nodes[i] = n.children[path[i-1]]
  end
  nodes
end

@dynamic! _p = Node[]
@dynamic! _i = 0

propagate(path::Vector, e) = begin
  i = length(path)
  while i > 0
    val = @dynamic! let _p = path, _i = i
      emit(path[i], e)
    end
    val === stop && break
    i -= 1
  end
end

propagate(path::Vector, name::Symbol, e) = begin
  i = length(path)
  while i > 0
    val = @dynamic! let _p = path, _i = i
      emit(path[i], name, e)
    end
    val === stop && break
    i -= 1
  end
end

"Invoke an event handler"
emit(node, e::Event) = emit(node, Events.name(e), e)
emit(node, name::Symbol, e) = get(node.attrs, name, e->nothing)(e)

"Emit a custom event that propagates from the location of the current event handler"
emit(s::Symbol, e) = begin
  path = _p[][1:_i[]]
  propagate(path, s, e)
end

"""
Return this value from an event handler to indicate that no more event handlers
should be called
"""
const stop = gensym(:stop_propagation)

const styles = Set{CSSNode}()

"""
Add some css to the global stylesheet and return a className so this css
can be applied to several DOM nodes. The `@dom` macro also has a special
case for this macro which allows you to write `@dom [:div css"color: red"]`
instead of `@dom [:div class=css"color: red"]`
"""
macro css_str(str)
  node = parse_css(str)
  push!(styles, node)
  css = IOBuffer()
  for s in styles
    show(css, "text/css", s)
  end
  stylesheets[1] = Container{:style}(empty_dict, [String(take!(css))])
  QuoteNode(Symbol('_', string(hash(node), base=16)))
end

"A CSS reset based on https://github.com/jgthms/minireset.css"
const reset = @dom[:style "html,body,p,ol,ul,li,dl,dt,dd,blockquote,figure,fieldset,legend,textarea,pre,iframe,hr,h1,h2,h3,h4,h5,h6{margin:0;padding:0}body{height:100%;width:100%;position:absolute}h1,h2,h3,h4,h5,h6{font-size:100%;font-weight:normal}ul{list-style:none}button,input,select,textarea{margin:0}html{box-sizing:border-box}*{box-sizing:inherit}*:before,*:after{box-sizing:inherit}img,embed,object,audio,video{height:auto;max-width:100%}iframe{border:0}table{border-collapse:collapse;border-spacing:0}td,th{padding:0;text-align:left}"]

"""
Sheets which all projects should put in their document head. `stylesheets[1]`
is where all the CSS generated by the `@css_str` macro ends up
"""
const stylesheets = [@dom[:style], reset]

"Defines an HTML document with the default stylesheets included"
HTML(attrs, children) =
  @dom[:html
    [:head stylesheets...]
    [:body{attrs...} children...]]

export @dom, @css_str, emit, diff, HTML
