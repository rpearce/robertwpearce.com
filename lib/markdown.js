'use strict'

const marked = require('marked')
const renderer = new marked.Renderer()
const { flip } = require('ramda')
const hljs = require('highlight.js')

renderer.heading = (text, level) => {
  const escapedText = text.toLowerCase().replace(/[^\w]+/g, '-');
  return `<h${level}>
    <a class="heading--link" name="${escapedText}" href="#${escapedText}">
      <span>${text}</span>
    </a>
  </h${level}>`
}

marked.setOptions({
  langPrefix: 'hljs ',
  highlight(code, lang) {
    return lang ? hljs.highlight(lang, code, true).value : code
  }
})

module.exports = flip(marked)({ renderer })
