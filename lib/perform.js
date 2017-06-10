'use strict'

const Task = require('data.task')
const { compose, curryN, map, sequence } = require('ramda')
const { copy, remove, mkdirp, outputFile } = require('./fs-task')

const seq = curryN(2, sequence)(Task.of)
const writePath = ({ path, content }) => outputFile(path, content)
const copyPath = ({ from, to }) => copy(from, to)

module.exports = ({ outputDir, copyable, writable }) =>
  remove(outputDir)
    .chain(_ => mkdirp(outputDir))
    .chain(_ => seq(map(copyPath)(copyable)))
    .chain(_ => seq(map(writePath)(writable)))
