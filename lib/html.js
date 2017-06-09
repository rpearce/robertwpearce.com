'use strict'

const { minify } = require('html-minifier')

const opts = {
  collapseWhitespace: true
}

module.exports = html => minify(html, opts)
