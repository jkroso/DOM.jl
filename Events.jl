const Events = current_module()

abstract Event

typealias DOMPath Vector{UInt8}

immutable Key{direction} <: Event
  target::DOMPath
  key::AbstractString
  mods::Set{Symbol}
end

typealias KeyUp Key{:up}
typealias KeyDown Key{:down}

abstract Mouse <: Event

immutable Wheel <: Mouse
  deltaX::UInt32
  deltaY::UInt32
end

immutable MouseMove <: Mouse
  x::UInt32
  y::UInt32
end

@enum MouseButton left middle right

immutable Button{direction} <: Mouse
  target::DOMPath
  button::MouseButton
  x::UInt32
  y::UInt32
end

typealias MouseDown Button{:down}
typealias MouseUp Button{:up}
typealias Click Button{:click}
typealias DoubleClick Button{:dblclick}

immutable HoverChange{direction} <: Mouse
  target::Vector{UInt8}
end

typealias MouseOut HoverChange{:out}
typealias MouseOver HoverChange{:over}

immutable FocusChange{direction} <: Event
  target::Vector{UInt8}
end

typealias Focus FocusChange{:in}
typealias Blur FocusChange{:out}

immutable Resize <: Event
  width::UInt32
  height::UInt32
end

immutable Scroll <: Event
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
  MouseMove(parse(UInt32, vals[1]),
            parse(UInt32, vals[2]))

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
