@require "github.com/MikeInnes/MacroTools.jl" => MacroTools @capture @match
@require "github.com/jkroso/Prospects.jl" group mapcat assoc @struct
@require "github.com/jkroso/write-json.jl"
@require "./Events" => Events Event
@require "./css" parse_css CSSNode

const runtime = joinpath(@dirname(), "runtime.js")

abstract type Node end
abstract type Primitive <: Node end
@struct Container{tag}(attrs::Dict{Symbol,Any}, children::Vector{Node}) <: Primitive
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

# handle a::Text,b::Container and vice versa
diff(a::Primitive, b::Primitive) = Nullable{Patch}(Replace(b))

diff(a::Text, b::Text) = Nullable{Patch}(a.value == b.value ? nothing : UpdateText(b.value))
diff(a::Container, b::Container) = Nullable{Patch}(Replace(b))
diff{tag}(a::Container{tag}, b::Container{tag}) = begin
  a === b && return Nullable{Patch}()
  m = Mutation(diff_attributes(a.attrs, b.attrs),
               diff_children(a.children, b.children))
  Nullable{Patch}(isempty(m) ? nothing : m)
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

Base.isempty(u::UpdateClassList) = isempty(u.remove) && isempty(u.add)
Base.isempty(u::UpdateStyle) = isempty(u.remove) && isempty(u.add)

diff_class(a::Set, b::Set) = UpdateClassList(setdiff(a, b), setdiff(b, a))

diff_style(a::Dict, b::Dict) =
  UpdateStyle(filter(k -> haskey(b, k), keys(a)),
              map(k-> k => b[k], filter(k -> get(a, k, nothing) == b[k], keys(b))))

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

macro dom(node) transform(node) end

transform(node::Any) = esc(node)
transform(node::AbstractString) = Text(node)
transform(node::Expr) = begin
  if node.head in [:vect :hcat :vcat]
    args = mapcat(n->isa(n, Expr) && n.head ≡ :row ? n.args : [n], node.args) # ignore rows
    tag, rest = (args[1], args[2:end])
    @capture(normalize_tag(tag), fn_(attrs__))
    extra_attrs, children = group(isattr, map(css_attr, rest))
    attrs = :(merge_attrs($(map(normalize_attr, [attrs..., extra_attrs...])...)))
    children = :(Node[$(map(transform, children)...)])
    :($fn($attrs, $children))
  else
    esc(node) # some sort of expression that generates a child node
  end
end

css_attr(x) = @capture(x, @css_str(_String)) ? :(:class => $x) : x
isattr(e) = @capture(e, (_ = _) | (_ => _))
normalize_attr(e) =
  @match e begin
    (a_.b_ = c_) => :($(QuoteNode(a)) => $(QuoteNode(b)) => $(esc(c)))
    ((:a_|a_) = b_) => :($(QuoteNode(a)) => $(esc(b)))
    _ => esc(e)
  end
normalize_tag(tag) =
  @match tag begin
    :tag_(attrs__) => :(Container{$(QuoteNode(tag))}($(attrs...)))
    tag_(attrs__) => :($(esc(tag))($(attrs...)))
    :tag_ => :(Container{$(QuoteNode(tag))}())
    tag_ => :($(esc(tag))())
    _ => error("unknown tag pattern $tag")
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

const empty_attrs = Base.ImmutableDict{Symbol,Any}()

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
    if isa(value, Bool)
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
                         Events.Blur=>:onblur,
                         Events.Resize=>:resize,
                         Events.Scroll=>:scroll)

"""
Invoke the handler of an event's target and then each of its parents
"""
dispatch{T<:Event}(n::Container, e::T) = begin
  name = event_names[T]
  path = Events.path(e)
  l = length(path)
  nodes = Vector{Node}(l)
  for i in 1:l
    nodes[i] = n
    n = n.children[path[i]]
  end
  # call handlers from most specific to least
  while true
    get(n.attrs, name, identity)(e)
    l == 0 && break
    n = nodes[l]
    l -= 1
  end
end

const styles = Set{CSSNode}()

macro css_str(string)
  node = parse_css(string)
  push!(styles, node)
  css = IOBuffer()
  for s in styles
    show(css, "text/css", s)
  end
  stylesheets[1] = Container{:style}(empty_attrs, [String(css)])
  QuoteNode(Symbol('_', hash(node) |> hex))
end

"A CSS reset based on https://github.com/jgthms/minireset.css"
const reset = @dom [:style "html,body,p,ol,ul,li,dl,dt,dd,blockquote,figure,fieldset,legend,textarea,pre,iframe,hr,h1,h2,h3,h4,h5,h6{margin:0;padding:0}body{height:100%;width:100%;position:absolute}h1,h2,h3,h4,h5,h6{font-size:100%;font-weight:normal}ul{list-style:none}button,input,select,textarea{margin:0}html{box-sizing:border-box}*{box-sizing:inherit}*:before,*:after{box-sizing:inherit}img,embed,object,audio,video{height:auto;max-width:100%}iframe{border:0}table{border-collapse:collapse;border-spacing:0}td,th{padding:0;text-align:left}"]

"""
Sheets which all projects should put in their document head. `stylesheets[1]`
is where all the CSS generated by the `@css_str` macro ends up

```julia
@dom [:html
  [:head stylesheets...]
  [:body]]
```
"""
const stylesheets = [@dom([:style]), reset]

export @dom, @css_str, stylesheets, Node, Container, Text
