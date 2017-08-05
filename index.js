'use strict'

const build = require('./lib/perform')
const buildBlogPages = require('./lib/blog')
const buildSass = require('./lib/sass')
const homePage = require('./src/js/pages/home')
const optimizeHtml = require('./lib/html')
const buildSitemap = require('./lib/sitemap')
const { hostname } = require('./src/config')

// Mutation hack...sigh, this is horrible
let css = ''
let posts = []
let sitemap = ''

buildSass('src/sass', 'src/sass/app.scss')
  .map(x => css = x)
  .chain(_ => buildBlogPages('src/blog'))
  .map(x => posts = x)
  .map(_ => sitemap = buildSitemap(hostname,
    [{ url: '/', changefreq: 'daily', priority: 1 }].concat(
      posts.map(x => ({ url: x.metadata.uri, changefreq: 'monthly', priority: 0.7 }))
    )
  ))
  .chain(_ => {
    return build({
      outputDir: 'docs',
      copyable: [
        { from: 'src/images', to: 'docs/images' },
        { from: 'src/CNAME', to: 'docs/CNAME' },
        { from: 'src/favicon.ico', to: 'docs/favicon.ico' }
      ],
      writable: [
        { path: 'docs/index.html', content: optimizeHtml(homePage({ posts })) },
        { path: 'docs/styles.css', content: css },
        { path: 'docs/sitemap.xml', content: sitemap }
      ].concat(posts.map(x => {
        return { path: `docs${x.metadata.relativePath}`, content: optimizeHtml(x.content) }
      }))
    })
  })
  .fork(
    err => console.error('Error: ', err),
    data => console.log('Build succeeded')
  )
