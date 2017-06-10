'use strict'

const path = require('path')
const sass = require('node-sass')
const taskFromFn = require('task-from-fn')
const { compose, map, prop, toString } = require('ramda')

const sassTask = taskFromFn(sass.render)

function setup(sassDir, sassEntry) {
  return sassTask({
    file: sassEntry,
    includePaths: [sassDir],
    outputStyle: 'compressed'
  })
}

const buildSass = compose(
  map(toString),
  map(prop('css')),
  setup
)

module.exports = buildSass
