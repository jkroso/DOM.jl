@use ".." @css_str @dom

function progress(attrs, children)
  @dom[:progress{attrs...}]
end
