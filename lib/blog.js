'use strict'

const { assoc, compose, lens, objOf, map, merge, prop, set, toString, view } = require('ramda')
const getSlug = require('speakingurl')
const parseMd = require('parse-md').default
const defaultData = require('../src/config')
const layout = require('../src/js/components/layout')
const article = require('../src/js/components/_article')
const { readFiles } = require('./fs-helpers')
const markdown = require('./markdown')

const getRelativeBlogPath = title => `/blog/${title}.html`

function convertBlogMarkdown(blog) {
  const fLens = lens(prop('content'), assoc('content'))
  const content = view(fLens, blog)
  const transformed = markdown(content)
  return set(fLens, transformed, blog)
}

function convertBlogMetadata(blog) {
  const fLens = lens(prop('metadata'), assoc('metadata'))
  const metadata = view(fLens, blog)
  const blogPath = compose(getRelativeBlogPath, getSlug)
  const blogPostPath = blogPath(metadata.title)
  const transformed = Object.assign({}, defaultData, metadata, {
    friendlyDate: getFriendlyDate(metadata.date),
    relativePath: blogPostPath,
    type: 'article',
    uri: defaultData.hostname.concat(blogPostPath)
  })
  return set(fLens, transformed, blog)
}

const getFriendlyDate = (date) => {
  const months = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'June', 'July', 'Aug', 'Sept', 'Oct', 'Nov', 'Dec'];
  const dateObj = new Date(date);
  const month = months[dateObj.getUTCMonth()];
  const day = dateObj.getUTCDate();
  const year = dateObj.getUTCFullYear();
  return `${month} ${day}, ${year}`;
}

function buildBlogPage({ metadata, content }) {
  const pageData = compose(
    merge({ metadata }),
    objOf('content'),
    layout
  )
  return pageData(
    Object.assign({}, defaultData, { body: article(metadata, content) }, metadata)
  )
}

const buildBlogPages = compose(
  map(map(buildBlogPage)),
  map(map(convertBlogMetadata)),
  map(map(convertBlogMarkdown)),
  map(map(parseMd)),
  map(map(toString)),
  readFiles
)

module.exports = buildBlogPages
