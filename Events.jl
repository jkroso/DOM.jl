const Events = current_module()
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

struct FocusChange{direction} <: Event
  target::Vector{UInt8}
end

const Focus = FocusChange{:in}
const Blur = FocusChange{:out}

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
  Type = getfield(Events, Symbol(vals[1]))
  parse_event(Type, vals[2:end])
end

parse_event(::Type{MouseMove}, vals::AbstractVector) =
  MouseMove(parse_byte_vector(vals[1]),
            parse(UInt32, vals[2]),
            parse(UInt32, vals[3]))

parse_event{d}(::Type{HoverChange{d}}, vals::AbstractVector) =
  HoverChange{d}(parse_byte_vector(vals[1]))

parse_event{d}(::Type{Button{d}}, vals::AbstractVector) =
  Button{d}(parse_byte_vector(vals[1]),
            MouseButton(parse(UInt8, vals[2])),
            parse(UInt32, vals[3]),
            parse(UInt32, vals[4]))

parse_event{d}(::Type{Key{d}}, vals::AbstractVector) =
  Key{d}(parse_byte_vector(vals[1]), vals[2], Set{Symbol}(map(Symbol, vals[3:end])))

parse_event{d}(::Type{FocusChange{d}}, vals::AbstractVector) =
  FocusChange{d}(parse_byte_vector(vals[1]))

parse_event(::Type{Resize}, vals::AbstractVector) =
  Resize(parse(UInt32, vals[1]), parse(UInt32, vals[2]))

parse_event(::Type{Scroll}, vals::AbstractVector) =
  Scroll(parse_byte_vector(vals[1]),
         parse(UInt32, vals[2]),
         parse(UInt32, vals[3]))

"Get the path of an Events target `Node`"
path(e::Event) = isdefined(e, :target) ? e.target : DOMPath()
