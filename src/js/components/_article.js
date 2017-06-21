'use strict'

const { curry } = require('ramda')
const nav = require('./_nav')
const subscribe = require('./_subscribe')

function article(metadata, body) {
  const { relativePath, title, friendlyDate, image } = metadata;

  return `<main>
    ${nav()}
    <article class="article">
      <div class="article__background" style="background-image: url('${image}')">
        <div class="overlay"></div>
        <header class="article__backgroundHeader">
          <div class="layout--constrained">
            <h1 class="heading--trim">
              <a href="${relativePath}">${title}</a>
            </h1>
            <small><em>${friendlyDate}</em></small>
          </div>
        </header>
      </div>
      <div class="layout--constrained">
        <header class="article__header heading--fatBottomBorder">
          <h1 class="heading--trim">
            <a href="${relativePath}">${title}</a>
          </h1>
          <small><em>${friendlyDate}</em></small>
        </header>
        ${body}
      </div>
    </article>
    ${subscribe()}
  </main>`;
}

module.exports = curry(article)
