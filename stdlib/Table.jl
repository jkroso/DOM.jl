@use ".." @css_str @dom Container Node

table(attrs, children) = begin
  @dom[:div{css"""
            border-radius: 3px
            background: white
            border: 1px solid #D0D0D0
            th {padding: 0.75rem 1.5rem; font: 1.3rem bolder, verdana}
            th,td {padding: 0.75rem 1.5rem}
            :where(tbody, thead) > tr > th {font: 1.3rem bolder, verdana}
            table
              width: 100%
              text-align: left
              position: relative
              overflow: auto
              tbody tr {border-bottom: 1px solid #D0D0D0}
              tbody tr:last-child {border-bottom: none}
              tfoot
                border-top: 1px solid #D0D0D0
                tr > th {text-align: right}
                tr > td {text-align: right}
                tr:not(:last-child) > th {font-weight: lighter; font-family: monospace; color: rgb(130,130,130)}
                tr:last-child > th {font-weight: bolder; font-family: monospace}
            """, attrs...}
    [:table children...]]
end

header(attrs, children) =
  if get(attrs,:index,true)
    @dom[:thead{attrs...} [:tr [:th] (@dom[:th x] for x in children)...]]
  else
    @dom[:thead{attrs...} [:tr (@dom[:th x] for x in children)...]]
  end
body(attrs, children) = @dom[:tbody{attrs...} (get(attrs,:index,true) ? map(add_index, enumerate(children)) : children)...]
add_index((i, tr)::Tuple{Number,Container}) = @dom[:tr{tr.attrs...} [:th string(i)] tr.children...]
footer(attrs, children) = @dom[:tfoot{attrs...} children...]

row(attrs, children) = @dom[:tr{attrs...} map(td, children)...]
td(x::Union{Container{:td}, Container{:th}}) = x
td(x::String) = @dom[:td x]
td(x::Node) = @dom[:td x]
