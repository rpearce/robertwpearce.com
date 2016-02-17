import { resolve } from 'url';
import { baseURI } from '../../config';

export default {
  render({ metadata, body }) {
    const { title, description, relativePath, image, type } = metadata;
    const uri = resolve(baseURI, relativePath);
    const feedURI = resolve(baseURI, 'feed.xml');

    return `<!DOCTYPE html>
      <html lang="en">
        <head>
          <title>${title}</title>

          <meta charset="utf-8">
          <meta http-equiv="X-UA-Compatible" content="IE=edge">
          <meta name="viewport" content="width=device-width, initial-scale=1, maximum-scale=1, minimum-scale=1">
          <meta name="google-site-verification" content="QX4bn65yOXBfgnwxlO09yIBc1H8blur-Erx3lmsDVhU">
          <meta name="description" content="${description}">

          <meta property="og:site_name" content="RobertWPearce.com">
          <meta property="og:url" content="${uri}">
          <meta property="og:description" content="${description}">
          <meta property="og:image" content="${image}">
          <meta property="og:type" content="${type}">

          <link href="http://fonts.googleapis.com/css?family=Lato:400" rel="stylesheet" type="text/css">
          <link rel="stylesheet" href="/css/app.css">
          <link rel="canonical" href="${uri}">
          <!--<link rel="alternate" type="application/rss+xml" title="Robert W. Pearce" href="${feedURI}">-->
        </head>
        <body>
          ${body}
        </body>
      </html>`;
  }
};
