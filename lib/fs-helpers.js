'use strict'

const path = require('path')
const Task = require('data.task')
const { readdir, readFile } = require('./fs-task')
const { compose, map, sequence, unnest } = require('ramda')

module.exports = {
  readFiles(dir) {
    const sequenceTasks = compose(
      unnest,
      map(sequence(Task.of))
    )

    const rebuildPath = x => path.join(dir, x)

    const readFileTasks = compose(
      map(map(readFile)),
      map(map(rebuildPath)),
      readdir
    )

    const readFiles = compose(
      sequenceTasks,
      readFileTasks
    )

    return readFiles(dir)
  }
}
