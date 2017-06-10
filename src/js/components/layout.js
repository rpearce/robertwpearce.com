'use strict'

module.exports = ({
  body=Function.prototype,
  canonicalUri='',
  cssPath='',
  description='',
  headExtra='',
  imageUri='',
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
      <meta property="og:image" content="${imageUri}">
      <meta property="og:type" content="${type}">

      <link rel="shortcut icon" href="/favicon.ico">
      <link rel="canonical" href="${canonicalUri || uri}">
      <link rel="stylesheet" href="${cssPath}">
      ${headExtra}
    </head>
    <body>
      ${body}
    </body>
  </html>
`
