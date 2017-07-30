'use strict'

const marked = require('marked')
const renderer = new marked.Renderer()
const prism = require('prismjs')
const { flip } = require('ramda')
require('../node_modules/prismjs/components/prism-bash')
require('../node_modules/prismjs/components/prism-c')
require('../node_modules/prismjs/components/prism-elixir')
require('../node_modules/prismjs/components/prism-haskell')
require('../node_modules/prismjs/components/prism-makefile')
require('../node_modules/prismjs/components/prism-ruby')
require('../node_modules/prismjs/components/prism-scss')

renderer.heading = (text, level) => {
  const escapedText = text.toLowerCase().replace(/[^\w]+/g, '-');
  return `<h${level}>
    <a class="heading--link" name="${escapedText}" href="#${escapedText}">
      <span>${text}</span>
    </a>
  </h${level}>`
}

marked.setOptions({
  highlight(code, lang) {
    const grammar = lang !== undefined ? prism.languages[lang] : prism.languages.markup
    return prism.highlight(code, grammar, lang)
  }
})

module.exports = flip(marked)({ renderer})
