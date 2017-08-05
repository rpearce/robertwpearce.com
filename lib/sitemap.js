'use strict'

const compose = require('ramda/src/compose')
const curryN = require('ramda/src/curryN')
const toString = require('ramda/src/toString')
const sm = require('sitemap')

const buildSitemap = curryN(2, (hostname, urls) =>
  sm.createSitemap({
    hostname,
    urls
  })
)

module.exports = compose(toString, buildSitemap)
