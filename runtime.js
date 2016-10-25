const assert = require("assert")

const create = (data) => {
  if (data.type == "Text") return document.createTextNode(data.value)
  const el = typeof createElement[data.tag] == 'function'
    ? createElement[data.tag](data.tag)
    : document.createElement(data.tag)
  const attrs = data.attrs
  for (const key in attrs) {
    if (key == "class")
      el.classList.add(...attrs[key])
    else
      setAttribute(el, key, attrs[key])
  }
  for (const child of data.children) {
    el.appendChild(create(child))
  }
  return el
}

/**
 * Some tags need special treatment
 */

const createElement = {
  svg() {
    const el = createSVG('svg')
    el.setAttribute('xmlns', 'http://www.w3.org/2000/svg')
    el.setAttribute('version', '1.1')
    el.setAttribute('height', '100%')
    el.setAttribute('width', '100%')
    return el
  }
}

const createSVG = tag => document.createElementNS('http://www.w3.org/2000/svg', tag)

;[
  "a",
  "altGlyph",
  "altGlyphDef",
  "altGlyphItem",
  "animate",
  "animateColor",
  "animateMotion",
  "animateTransform",
  "animation",
  "audio",
  "canvas",
  "circle",
  "clipPath",
  "color-profile",
  "cursor",
  "defs",
  "desc",
  "discard",
  "ellipse",
  "feBlend",
  "feColorMatrix",
  "feComponentTransfer",
  "feComposite",
  "feConvolveMatrix",
  "feDiffuseLighting",
  "feDisplacementMap",
  "feDistantLight",
  "feDropShadow",
  "feFlood",
  "feFuncA",
  "feFuncB",
  "feFuncG",
  "feFuncR",
  "feGaussianBlur",
  "feImage",
  "feMerge",
  "feMergeNode",
  "feMorphology",
  "feOffset",
  "fePointLight",
  "feSpecularLighting",
  "feSpotLight",
  "feTile",
  "feTurbulence",
  "filter",
  "font",
  "font-face",
  "font-face-format",
  "font-face-name",
  "font-face-src",
  "font-face-uri",
  "foreignObject",
  "g",
  "glyph",
  "glyphRef",
  "handler",
  "hatch",
  "hatchpath",
  "hkern",
  "iframe",
  "image",
  "line",
  "linearGradient",
  "listener",
  "marker",
  "mask",
  "mesh",
  "meshpatch",
  "meshrow",
  "metadata",
  "missing-glyph",
  "mpath",
  "path",
  "pattern",
  "polygon",
  "polyline",
  "prefetch",
  "radialGradient",
  "rect",
  "script",
  "set",
  "solidColor",
  "solidcolor",
  "stop",
  "style",
  "svg",
  "switch",
  "symbol",
  "tbreak",
  "text",
  "textArea",
  "textPath",
  "title",
  "tref",
  "tspan",
  "unknown",
  "use",
  "video",
  "view",
  "vkern"
].forEach(tag => createElement[tag] = createSVG)

/**
 * Set an attribute on `el`
 *
 * @param {Node} el
 * @param {String} key
 * @param {Any} value
 */

const setAttribute = (el, key, value) => {
  if (key == 'style') {
    for (key in value) el.style[key] = value[key]
  } else if (key == 'isfocused') {
    // Since HTML doesn't specify an isfocused attribute we fake it
    if (value) setTimeout(() => el.focus())
  } else if (key == 'value') {
    // often value has already updated itself
    if (el.value != value) el.value = value
  } else if (key == 'class') {
    for (key in value) {
      el.classList.toggle(key, value[key])
    }
  } else {
    if (typeof value == 'boolean') el[key] = value
    else el.setAttribute(key, value)
  }
}

/**
 * Apply a patch to the DOM
 *
 * @param {Object} data
 * @param {Element} dom
 */

const patch = (data, dom) => commands[data.command](data, dom)
const commands = {
  UpdateText({value}, dom) {
    dom.nodeValue = value
  },
  SetAttribute({key, value}, dom) {
    setAttribute(dom, key, value)
  },
  UpdateStyle({remove, add}, {style}) {
    for (const key of remove) style[key] = '' // `delete` doesn't always work
    for (const [key,value] of add) style[key] = value
  },
  UpdateClassList({remove, add}, {classList}) {
    for (const c of remove) classList.remove(c)
    for (const c of add) classList.add(c)
  },
  RemoveAttribute({attr}, dom) {
    dom.removeAttribute(attr)
  },
  Replace({node}, dom) {
    dom.parentElement.replaceChild(create(node), dom)
  },
  Mutation({attrs, children}, dom) {
    for (const p of attrs) patch(p, dom)
    for (let i = 0, len = children.length; i < len;) {
      const child = children[i]
      if (child.command == "Skip") {
        i += child.n
        continue
      }
      if (child.command == "TrimChildren") {
        var n = child.n
        while (n-- > 0) dom.removeChild(dom.lastChild)
        break
      }
      if (child.command == "AppendChildren") {
        child.nodes.forEach(node => dom.appendChild(create(node)))
        break
      }
      patch(child, dom.childNodes[i++])
    }
  }
}

const init = (data) => {
  assert(data.tag == "html")
  for (const node of data.children) {
    const dom = node.tag == "head" ? document.head : document.body
    dom.innerHTML = ""
    for (const child of node.children) {
      dom.appendChild(create(child))
    }
  }
}

const mutate = (data) => {
  patch(data, document.lastChild)
}

exports.init = init
exports.mutate = mutate
