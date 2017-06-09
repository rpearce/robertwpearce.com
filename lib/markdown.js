'use strict'

const marked = require('marked')
const prism = require('prismjs')
require('../node_modules/prismjs/components/prism-ruby')
require('../node_modules/prismjs/components/prism-elixir')
require('../node_modules/prismjs/components/prism-scss')
require('../node_modules/prismjs/components/prism-bash')
require('../node_modules/prismjs/components/prism-makefile')
require('../node_modules/prismjs/components/prism-c')

marked.setOptions({
  highlight(code, lang) {
    const grammar = lang !== undefined ? prism.languages[lang] : prism.languages.markup
    return prism.highlight(code, grammar, lang)
  }
})

module.exports = marked
