# DOM.jl

Enables you to map an HTML/SVG DOM into Julia land. To render a UI as a web app you send a JSON encoding of it to the [runtime](/runtime.js). The runtime will interpret it and send any UI events back. When you want to update the UI you recreate the whole UI (probably using the same function that created the initial UI so its not hard) and `diff` it against the old UI tree. This generates a `Patch` which you can send to the runtime and have it applied to web app.

[PatchWork.jl](https://github.com/shashi/Patchwork.jl) does something very similar but doesn't deal with events

This package requires a bit of wiring up in order to use. For an example on how to do that see [Rutherford](https://github.com/jkroso/Rutherford.jl). Basically the idea is to create a TCP connection between your Julia process and the browser. And use this to send `Patch`s from Julia into the browser and to send events from the browser into Julia

## API

The easiest way to create UI is with the `@dom <node>` macro. The syntax:

```
<node>: String | [<tag> <attribute>... <node>...]
<attribute>: Symbol=Any
<tag>: :tag_name | Function
```

```julia
doc = @dom [:html
  [:head [:style "p {color: red}"]]
  [:body [:p class="spinner" "Loading"]]]
```

### diff(a::Node, b::Node)

Produces a `Patch` which describes how to transform `a` into `b`
