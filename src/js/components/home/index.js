'use strict'

const reviewList = require('./_reviews')
const postList = require('./_posts')
const subscribe = require('../_subscribe')

module.exports = ({ posts, reviews }) => `
  <main>
    <header class="author">
      <div class="layout--constrained">
        <img class="author__image" src="/images/robert-london.jpg" alt="Robert Pearce" title="Robert Pearce in London" />
        <h1 class="author__name">Robert Pearce</h1>
        <h2 class="author__title">Freelance Software Developer</h2>
        <div class="author__links">
          <ul class="list--bare list--horz nav--main">
            <li><a href="mailto:me@robertwpearce.com?subject=Work Inquiry">Hire Me</a></li>
            <li><a href="https://github.com/rpearce">GitHub</a></li>
            <li><a href="https://www.linkedin.com/in/robertwpearce">LinkedIn</a></li>
            <li><a href="https://dribbble.com/rpearce">dribbble</a></li>
          </ul>
        </div>
      </div>
    </header>
    <section class="spaceUp">
      <div class="layout--constrained">
        <h2 class="heading--bordered">Recommendations</h2>
        ${reviewList(reviews)}
      </div>
    </section>
    ${subscribe()}
    <section class="spaceUp">
      <div class="layout--constrained">
        <h2 class="heading">Blog Posts</h2>
        ${postList(posts)}
      </div>
    </section>
  </main>
`
