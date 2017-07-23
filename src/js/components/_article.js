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
        <header class="article__bgHeader">
          <div class="layout--constrained">
            <div class="article__bgTitleContainer">
              <h1 class="heading--trim">
                <a href="${relativePath}">${title}</a>
              </h1>
              <div class="article__subHeader">
                <small>${friendlyDate}</small>
                ${photoCred}
              </div>
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
