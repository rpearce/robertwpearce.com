'use strict'

module.exports = ({
  body=Function.prototype,
  canonicalUri='',
  cssPath='',
  description='',
  headExtra='',
  footerExtra='',
  image='',
  lang='en',
  siteName='',
  title='',
  type='',
  uri=''
}) => `<!DOCTYPE html>
  <html lang="${lang}">
    <head>
      <title>${title}</title>
      <meta charset="utf-8">
      <meta name="viewport" content="width=device-width, initial-scale=1, maximum-scale=1, minimum-scale=1">
      <meta name="description" content="${description}">

      <meta property="og:site_name" content="${siteName}">
      <meta property="og:title" content="${title}">
      <meta property="og:url" content="${uri}">
      <meta property="og:description" content="${description}">
      <meta property="og:image" content="${image}">
      <meta property="og:type" content="${type}">

      <meta property="twitter:card" content="summary_large_image">
      <meta property="twitter:site" content="${siteName}">
      <meta property="twitter:title" content="${title}">
      <meta property="twitter:description" content="${description}">
      <meta property="twitter:image" content="${image}">

      <link rel="shortcut icon" href="/favicon.ico">
      <link rel="canonical" href="${canonicalUri || uri}">
      <link rel="stylesheet" href="${cssPath}">
      ${headExtra}
    </head>
    <body>
      ${body}
      ${footerExtra}
    </body>
  </html>
`
