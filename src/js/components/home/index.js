'use strict'

const reviewList = require('./_reviews')
const postList = require('./_posts')

module.exports = ({ posts, reviews }) => `
  <main>
    <div class="layout--constrained">
      <header class="author">
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
      </header>
      <section class="spaceUp">
        <header>
          <h2 class="heading--bordered">Recommendations</h2>
        </header>
        ${reviewList(reviews)}
      </section>
      <section class="spaceUp">
        <header>
          <h2 class="heading--bordered">Blog Posts</h2>
        </header>
        ${postList(posts)}
      </section>
    </div>
  </main>
`
