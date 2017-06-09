'use strict'

const layout = require('../components/layout')
const page = require('../components/home')
const { reviews } = require('../components/home/reviews')
const defaultData = require('../../config')
const { merge } = require('ramda')

module.exports = ({ posts }) => layout(merge(defaultData, {
  body: page({ posts, reviews })
}))
