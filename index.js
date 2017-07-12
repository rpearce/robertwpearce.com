'use strict'

const build = require('./lib/perform')
const buildBlogPages = require('./lib/blog')
const buildSass = require('./lib/sass')
const homePage = require('./src/js/pages/home')
const optimizeHtml = require('./lib/html')

// Mutation hack
let css = ''

buildSass('src/sass', 'src/sass/app.scss')
  .map(x => css = x)
  .chain(_ => buildBlogPages('src/blog'))
  .chain(posts => {
    return build({
      outputDir: 'docs',
      copyable: [
        { from: 'src/images', to: 'docs/images' },
        { from: 'src/CNAME', to: 'docs/CNAME' },
        { from: 'src/favicon.ico', to: 'docs/favicon.ico' }
      ],
      writable: [
        { path: 'docs/index.html', content: optimizeHtml(homePage({ posts })) },
        { path: 'docs/styles.css', content: css }
      ].concat(posts.map(x => {
        return { path: `docs${x.metadata.relativePath}`, content: optimizeHtml(x.content) }
      }))
    })
  })
  .fork(
    err => console.error('Error: ', err),
    data => console.log('Build succeeded')
  )
