@require "./style" parse_css CSSNode
@require "." Container

const stylesheet = Container{:style}(Dict(), [])
const styles = Set{CSSNode}()

Base.show(io::IO, m::MIME"text/html", n::Container{:style}) = begin
  if n === stylesheet
    write(io, "<style>")
    for s in styles
      show(io, "text/css", s)
    end
    write(io, "</style>")
  else
    invoke(show, (IO, MIME"text/html", Container), io, m, n)
  end
end

macro css_str(string)
  quote
    node = parse_css(IOBuffer($string))
    push!(styles, node)
    hash(node) |> hex
  end
end
