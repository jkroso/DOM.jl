const DOMPath = Vector{UInt8}

abstract type Event end

struct Key{direction} <: Event
  target::DOMPath
  key::AbstractString
  mods::Set{Symbol}
end

const KeyUp = Key{:up}
const KeyDown = Key{:down}

abstract type Mouse <: Event end

struct Wheel <: Mouse
  deltaX::UInt32
  deltaY::UInt32
end

struct MouseMove <: Mouse
  target::DOMPath
  x::UInt32
  y::UInt32
end

@enum MouseButton left middle right

struct Button{direction} <: Mouse
  target::DOMPath
  button::MouseButton
  x::UInt32
  y::UInt32
end

const MouseDown = Button{:down}
const MouseUp = Button{:up}
const Click = Button{:click}
const DoubleClick = Button{:dblclick}

struct HoverChange{direction} <: Mouse
  target::Vector{UInt8}
end

const MouseOut = HoverChange{:out}
const MouseOver = HoverChange{:over}

struct Resize <: Event
  width::UInt32
  height::UInt32
end

struct Scroll <: Event
  target::Vector{UInt8}
  x::UInt32
  y::UInt32
end

parse_byte_vector(s::AbstractString) = begin
  str = s[2:end-1]
  isempty(str) ? UInt8[] : map(n->parse(UInt8, n), split(str, ','))
end

parse_event(s::AbstractString) = begin
  vals = split(rstrip(s), ' ')
  Type = getfield(@__MODULE__(), Symbol(vals[1]))
  parse_event(Type, vals[2:end])
end

parse_event(::Type{MouseMove}, vals::AbstractVector) =
  MouseMove(parse_byte_vector(vals[1]),
            parse(UInt32, vals[2]),
            parse(UInt32, vals[3]))

parse_event(::Type{HoverChange{d}}, vals::AbstractVector) where d =
  HoverChange{d}(parse_byte_vector(vals[1]))

parse_event(::Type{Button{d}}, vals::AbstractVector) where d =
  Button{d}(parse_byte_vector(vals[1]),
            MouseButton(parse(UInt8, vals[2])),
            parse(UInt32, vals[3]),
            parse(UInt32, vals[4]))

parse_event(::Type{Key{d}}, vals::AbstractVector) where d =
  Key{d}(parse_byte_vector(vals[1]),
         vals[2] == "Space" ? " " : vals[2],
         Set{Symbol}(map(Symbol, vals[3:end])))

parse_event(::Type{Resize}, vals::AbstractVector) =
  Resize(parse(UInt32, vals[1]), parse(UInt32, vals[2]))

parse_event(::Type{Scroll}, vals::AbstractVector) =
  Scroll(parse_byte_vector(vals[1]),
         parse(UInt32, vals[2]),
         parse(UInt32, vals[3]))

"Get the path of an Events target `Node`"
path(e::Event) = isdefined(e, :target) ? e.target : DOMPath()

const event_names = Dict(KeyUp=>:onkeyup,
                         KeyDown=>:onkeydown,
                         MouseMove=>:onmousemove,
                         MouseDown=>:onmousedown,
                         MouseUp=>:onmouseup,
                         Click=>:onclick,
                         DoubleClick=>:ondblclick,
                         MouseOut=>:onmouseout,
                         MouseOver=>:onmouseover,
                         Resize=>:onresize,
                         Scroll=>:onscroll)

"""
Get the canonical name of an Event object
"""
name(e::Event) = event_names[typeof(e)]
