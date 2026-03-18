@use ".." => DOM @dom @css_str Node Container Attrs add_attr ["html.jl"]
@use "github.com/jkroso/Prospects.jl" assoc interleave
@use "./Markdown.jl" renderMD
import Dates
import Markdown
using InteractiveUtils

"fallback rendering method that ignores context and intent"
function dom end

"Formats long numbers with commas separating it into chunks"
seperate(value::Number; kwargs...) = seperate(string(convert(Float64, value)), kwargs...)
seperate(value::Integer; kwargs...) = seperate(string(value), kwargs...)
seperate(str::String, sep = ",", k = 3) = begin
  parts = split(str, '.')
  int = parts[1]
  uint = split(int, '-')[end]
  groups = (uint[max(x-k+1, 1):x] for x in reverse(length(uint):-k:1))
  whole_part = @dom[:span startswith(int, '-') ? "-" : "" interleave(groups, @dom[:span css"color: grey" sep])...]
  length(parts) == 1 && return whole_part
  @dom[:span whole_part @dom[:span css"color: grey" "."] parts[2]]
end

vstack(attrs, children) = @dom[:div{attrs...} css"display: flex; flex-direction: column" children...]
hstack(attrs, children) = @dom[:div{attrs...} css"display: flex; flex-direction: row" children...]

syntax(x) = @dom[:span class="syntax--language syntax--julia" class=syntax_class(x) repr(x)]
syntax_class(::Bool) = ["syntax--constant", "syntax--boolean"]
syntax_class(::Number) = ["syntax--constant", "syntax--numeric"]
syntax_class(::AbstractString) = ["syntax--string", "syntax--quoted", "syntax--double"]
syntax_class(::Regex) = ["syntax--string", "syntax--regexp"]
syntax_class(::Symbol) = ["syntax--constant", "syntax--other", "syntax--symbol"]
syntax_class(::Char) = ["syntax--string", "syntax--quoted", "syntax--single"]
syntax_class(::VersionNumber) = ["syntax--string", "syntax--quoted", "syntax--other"]
syntax_class(::Nothing) = ["syntax--constant"]
syntax_class(::Function) = ["syntax--support", "syntax--function"]
syntax_class(::Missing) = []

fade(s) = @dom[:span class="fade" s]

stripparams(t) = replace(t, r"\{([A-Za-z, ]*?)\}"=>"")
interpose(xs, y) = map(i -> iseven(i) ? xs[i÷2] : y, 2:2length(xs))

expandable(head, body) =
  @dom[:details css"""
               > summary {cursor: pointer; list-style: none}
               > summary::-webkit-details-marker {display: none}
               > summary > .chevron {transition: transform 0.1s ease-out; display: inline-block}
               &[open] > summary > .chevron {transform: rotate(0.25turn)}
               """
    [:summary [:span class="chevron" "\u25b6 "] head]
    [:div css"padding: 0 0 3px 20px; overflow: auto; max-height: 500px" body]]

# Basic types

dom(n::Union{AbstractFloat,Integer}) = @dom[:span class="syntax--language syntax--julia syntax--constant syntax--numeric" seperate(n)]
dom(n::Unsigned) = @dom[:span class="syntax--language syntax--julia syntax--constant syntax--numeric" repr(n)]
dom(x::Union{Regex,Symbol,Char,VersionNumber,Nothing,Number,Missing}) = syntax(x)
dom(b::Bool) = syntax(b)
dom(d::Dates.Date) = @dom[:span Dates.format(d, Dates.dateformat"dd U Y")]
dom(d::Dates.DateTime) = @dom[:span Dates.format(d, Dates.dateformat"dd/mm/Y H\h M\m S.s\s")]
dom(d::Dates.Time) = @dom[:span Dates.format(d, Dates.dateformat"HH:MM:S.s")]

dom(s::AbstractString) =
  if occursin(r"\n", s)
    @dom[vstack class="syntax--string syntax--quoted syntax--triple syntax--double syntax--julia"
      [:span "\"\"\""]
      [:span css"white-space: pre" s "\"\"\""]]
  else
    syntax(s)
  end

# Rationals

dom(r::Rational) = begin
  whole, part = divrem(r.num, r.den)
  if whole == 0
    fraction(r)
  else
    @dom[:span css"> :first-child {margin-right: 2px}" dom(whole) fraction(part//r.den)]
  end
end

fraction(r::Rational) =
  @dom[:span css"""
             display: inline-flex
             flex-direction: column
             line-height: 1em
             vertical-align: middle
             > .syntax--numeric {font-size: 0.8em}
             > :first-child {border-bottom: 1px solid rgb(185,185,185); width: 100%; text-align: center}
             """
    dom(r.num) dom(r.den)]

# Pair

dom(p::Pair) =
  @dom[:span dom(p.first) [:span css"padding: 0 6px" "=>"] dom(p.second)]

# Ranges

dom(r::AbstractRange) =
  @dom[:span class="syntax--language syntax--julia syntax--constant syntax--numeric" repr(r)]

# Complex

dom(z::Complex) = begin
  r, i = reim(z)
  @dom[:span dom(r)
    [:span css"padding: 0 2px" i < 0 ? "-" : "+"]
    dom(abs(i))
    [:span class="syntax--constant syntax--language" "im"]]
end

# Expr

dom(e::Expr) =
  @dom[:span css"font-family: monospace; white-space: pre" class="syntax--language syntax--julia" string(e)]

# Exceptions

dom(e::Exception) =
  @dom[:span css"color: red" [:strong typeof(e) |> string ": "] sprint(showerror, e)]

# RegexMatch

dom(m::RegexMatch) =
  @dom[vstack
    [:span [:span class="syntax--string syntax--regexp" repr(m.regex)] " matched"]
    [:span css"font-weight: bold" repr(m.match)]
    if !isempty(m.captures)
      @dom[vstack css"padding-left: 1em"
        (@dom[hstack [:span css"color: grey" string(i) ": "] dom(c)]
         for (i, c) in enumerate(m.captures))...]
    end]

# Cmd

dom(c::Cmd) =
  @dom[:span class="syntax--string syntax--quoted" css"font-family: monospace" string(c)]

# Some

dom(s::Some) = @dom[:span "Some(" dom(something(s)) ")"]

# Dates.Period

dom(p::Dates.Period) = @dom[:span dom(Dates.value(p)) " " string(typeof(p))]

# AbstractSet (covers BitSet etc.)

dom(s::AbstractSet) = begin
  isempty(s) && return @dom[:span brief(typeof(s)) [:span css"color: rgb(104, 110, 122)" "[0]"]]
  expandable(
    @dom[:span brief(typeof(s)) [:span css"color: rgb(104, 110, 122)" "[$(length(s))]"]],
    @dom[vstack (@dom[:span dom(x)] for x in s)...])
end

# Enums

dom(e::Enum) = @dom[:span repr(e)]
dom(E::Type{<:Enum}) = @dom[:span repr(E)]

# Type system

brief(T::DataType) = begin
  @dom[:span
    [:span class="syntax--support syntax--type" T.name.name]
    if !isempty(T.parameters)
      @dom[:span css"display: inline-flex; flex-direction: row"
        [:span "{"] interleave(map(brief, T.parameters), ",")... [:span "}"]]
    end]
end

brief(x::UnionAll) = begin
  body, = flatten_unionall(x)
  @dom[:span
    [:span class="syntax--support syntax--type" body.name.name]
    [:span "{" interleave(map(brief_param, body.parameters), ",")... "}"]]
end
brief_param(t::TypeVar) = @dom[:span class="syntax--keyword syntax--operator syntax--relation syntax--julia" "<:" brief(t.ub)]
brief_param(x) = brief(x)

brief(t::TypeVar) = @dom[:span t.name]
brief(s::Symbol) = dom(s)
brief(n::Union{Number,Char}) = syntax(n)
brief(e::Enum) = dom(e)

brief(u::Union) = @dom[:span
  [:span class="syntax--support syntax--type" "Union"]
  [:span "{"]
  interleave(map(brief, union_params(u)), ",")...
  [:span "}"]]

union_params(u::Union) = push!(union_params(u.b), u.a)
union_params(u) = Any[u]

flatten_unionall(x::DataType, vars=[]) = x, vars
flatten_unionall(x::UnionAll, vars=[]) = flatten_unionall(x.body, push!(vars, x.var))

dom(t::TypeVar) = begin
  t.ub == Any && return @dom[:span t.name]
  @dom[:span t.name [:span class="syntax--keyword syntax--operator syntax--relation syntax--julia" "<:"] brief(t.ub)]
end

header(T::DataType) = begin
  if supertype(T) ≠ Any
    @dom[:span
      brief(T)
      [:span class="syntax--keyword syntax--operator syntax--relation syntax--julia" " <: "]
      brief(supertype(T))]
  else
    brief(T)
  end
end

header(x::UnionAll) = begin
  body, vars = flatten_unionall(x)
  vars = map(dom, vars)
  @dom[:span header(body)
    [:span class="syntax--keyword syntax--other" css"padding: 0 0.5em" "where"]
    length(vars) == 1 ? vars[1] : @dom[:span "{" interleave(vars, ",")... "}"]]
end

unwrap(u::UnionAll) = unwrap(u.body)
unwrap(u::DataType) = u

fields(T) = try fieldnames(T) catch; () end

dom(T::Union{DataType,UnionAll}) = begin
  attrs = fields(T)
  isempty(attrs) && return header(T)
  expandable(header(T),
    @dom[vstack
      [vstack css"padding: 3px 5px; background: white; border-radius: 3px; margin: 3px 0"
        (@dom[hstack
          [:span String(name)]
          [:span "::"]
          [:span brief(fieldtype(unwrap(T), name))]]
        for name in attrs)...]
      expandable(@dom[:h4 "Constructors"],
        @dom[vstack (render_method(m, name=@dom[:span class="syntax--support syntax--function" unwrap(T).name.name]) for m in methods(T))...])])
end

dom(::Type{T}) where T <: Tuple = brief(T)
dom(u::Type{Union{}}) = @dom[:span "Union{}"]
dom(u::Union) = brief(u)

# Functions and methods

name(m::Base.MethodList) = @dom[:span class="syntax--support syntax--function" string(m.mt.name)]
name(m::Method) = @dom[:span class="syntax--support syntax--function" string(m.name)]
isanon(f) = occursin('#', String(nameof(f)))
name(f::Function) = @dom[:span class=syntax_class(f) isanon(f) ? "λ" : String(nameof(f))]

render_method(m::Method; name=name(m)) = begin
  tv, decls, file, line = Base.arg_decl_parts(m)
  params = [@dom[:span x isempty(T) ? "" : "::" [:span class="syntax--support syntax--type" stripparams(T)]]
            for (x, T) in decls[2:end]]
  sig = @dom[:span name "(" interpose(params, ", ")... ")"]
  link = @dom[:span css"opacity: 0.6" string(file) ":" string(line)]
  @dom[:span sig " at " link]
end

dom(m::Method) = render_method(m)

dom(m::Base.MethodList) = begin
  ms = collect(m)
  isempty(ms) && return @dom[:span name(m) " has no methods"]
  length(ms) == 1 && return dom(ms[1])
  expandable(@dom[:span name(m) " has $(length(ms)) methods"],
    @dom[vstack (dom(method) for method in m)...])
end

dom(f::Function) = begin
  ms = methods(f)
  expandable(name(f),
    @dom[vstack (dom(method) for method in ms)...])
end

# Modules

brief(m::Module) = @dom[:span class="syntax--keyword syntax--other" replace(repr(m), r"^Main\."=>"")]
issubmodule(m::Module) = parentmodule(m) != m && parentmodule(m) != Main

dom(m::Module) = begin
  header = if issubmodule(m)
    brief(m)
  else
    file = pathof(m)
    @dom[:span brief(m) if file != nothing; @dom[:span " from " file] end]
  end
  expandable(header,
    @dom[vstack
      (@dom[hstack
        [:span String(n)]
        [:span css"padding: 0 10px" "→"]
        isdefined(m, n) ? dom(getproperty(m, n)) : fade("#undef")]
      for n in names(m, all=true) if !occursin('#', String(n)) && n != nameof(m))...])
end

# Collections

brief(data::Union{AbstractDict,AbstractVector,Set,Tuple,NamedTuple}) =
  @dom[:span brief(typeof(data)) [:span css"color: rgb(104, 110, 122)" "[$(length(data))]"]]

brief(data::T) where T = @dom[:span brief(T) '[' length(propertynames(data)) ']']

dom(x::Union{AbstractDict,AbstractVector,Set}) = begin
  isempty(x) && return brief(x)
  expandable(brief(x), body(x))
end

body(dict::AbstractDict) =
  @dom[:div
    (@dom[:div css"display: flex"
      dom(key)
      [:span css"padding: 0 10px" "→"]
      dom(value)]
    for (key, value) in dict)...]

body(v::AbstractVector) =
  @dom[vstack (@dom[:span dom(x)] for x in v)...]

body(s::Set) = @dom[vstack (@dom[:span dom(x)] for x in s)...]

dom(t::NamedTuple) = length(t) < 5 ? literal(t) : expandable(brief(t), body(t))
dom(t::Tuple) = length(t) < 10 ? literal(t) : expandable(brief(t), body(t))

literal(t::Tuple) = begin
  items = map(dom, t)
  content = collect(interleave(items, @dom[:span css"padding: 0 6px 0 0" ',']))
  length(t) == 1 && push!(content, @dom[:span ','])
  @dom[hstack [:span '('] content... [:span ')']]
end

literal(t::NamedTuple) = begin
  items = (@dom[hstack string(k) '=' dom(t[k])] for k in keys(t))
  content = collect(DOM.Node, interleave(items, @dom[:span css"padding: 0 6px 0 0" ',']))
  length(t) == 1 && push!(content, @dom[:span ','])
  @dom[hstack [:span '('] content... [:span ')']]
end

brief(nt::NamedTuple) =
  @dom[:span
    [:span class="syntax--support syntax--type" "NamedTuple"]
    [:span css"color: rgb(104, 110, 122)" "[$(length(nt))]"]]

body(nt::NamedTuple) =
  @dom[vstack
    (@dom[hstack String(key) [:span css"padding: 0 5px" "="] dom(nt[key])] for key in keys(nt))...]

body(t::Tuple) = @dom[vstack (@dom[:span dom(x)] for x in t)...]

# Matrix

dom(a::AbstractMatrix) =
  @dom[:table css"""
              display: grid
              border: 1px solid lightgrey
              margin: 0.5em 0
              border-radius: 5px
              background: rgb(250,250,250)
              td {padding: 0.4em 1em}
              tr:nth-child(even) {background: rgb(235,235,235)}
              tr > td:first-child {padding-left: 1em}
              """
    [:tbody
      (@dom[:tr (@dom[:td dom(x)] for x in row)...] for row in eachrow(a))...]]

# BitVector

dom(bits::BitVector) = @dom[:span "BitVector[" dom(length(bits)) "] " map(dom∘Int, bits)...]

# StackTrace

dom(trace::StackTraces.StackTrace) = begin
  @dom[:div class="error-trace"
    map(trace) do frame
      @dom[:div class="trace-entry"
        fade("in ")
        [:span string(frame.func)]
        fade(" at ")
        [:span css"opacity: 0.8" String(frame.file) ":" string(frame.line)]
        fade(frame.inlined ? " <inlined>" : "")]
    end...]
end

# Markdown

dom(m::Markdown.MD) =
  @dom[:div
    css"""
    max-width: 50em
    margin: 0 auto
    padding: 1.5em
    white-space: normal
    font: 1em/1.6em helvetica-light, sans-serif
    code.inline
      font-family: monospace
      border-radius: 3px
      padding: 0px 8px
      background: #f9f9f9
      color: rgb(35,35,35)
      border: 1px solid #e8e8e8
    h1, h2, h3, h4
      font-weight: 600
      margin: 0.5em 0
    h1 {font-size: 2em; margin: 1.5em 0}
    h2 {font-size: 1.5em}
    h3 {font-size: 1.25em}
    ul, ol
      margin: 0
      padding-left: 2em
      li {line-height: 1.5em; margin: 0.4em 0}
      li p {margin: 0}
      ul > li {list-style: circle; margin: 0}
      ul > li > *, ol > li > * {margin: 0}
    blockquote
      padding: 0 1em
      color: #6a737d
      border-left: .25em solid #dfe2e5
    p, blockquote {margin-bottom: 1em}
    """
    map(renderMD, m.content)...]

# Generic fallback

dom(data::T) where T = begin
  if showable("text/html", data)
    parse(MIME("text/html"), sprint(show, MIME("text/html"), data))
  else
    attrs = propertynames(data)
    isempty(attrs) && return brief(T)
    expandable(brief(data),
      @dom[vstack
        (@dom[hstack
          [:span string(field)]
          [:span css"padding: 0 10px" "→"]
          hasproperty(data, field) ? dom(getproperty(data, field)) : fade("#undef")]
        for field in attrs)...])
  end
end
