'use strict'

const { curry } = require('ramda')
const nav = require('./_nav')
const subscribe = require('./_subscribe')

function article(metadata, body) {
  const { relativePath, title, friendlyDate, image, photoCredit, photoWebsite } = metadata;
  const photoCred = photoCredit
    ? `<small class="photoCred">\[photo credit <a href="${photoWebsite}">${photoCredit}</a>\]</small>`
    : ''

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
            <div class="article__subHeader">
              <small>${friendlyDate}</small>
              ${photoCred}
            </div>
          </div>
        </header>
      </div>
      <div class="layout--constrained">
        <header class="article__header heading--fatBottomBorder">
          <h1 class="heading--trim">
            <a href="${relativePath}">${title}</a>
          </h1>
          <small>${friendlyDate}</small>
        </header>
        ${body}
      </div>
    </article>
    ${subscribe()}
  </main>`;
}

module.exports = curry(article)
