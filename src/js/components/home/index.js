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
        <section>
          <!-- Begin MailChimp Signup Form -->
          <form action="//robertwpearce.us13.list-manage.com/subscribe/post?u=2df44e8960266388bff165fa6&amp;id=ee2f2a3737" method="post" name="mc-embedded-subscribe-form" target="_blank" novalidate class="form">
            <div id="mce-responses">
              <div class="response" id="mce-error-response" style="display:none"></div>
              <div class="response" id="mce-success-response" style="display:none"></div>
            </div>    <!-- real people should not fill this in and expect good things - do not remove this or risk form bot signups-->
            <div style="position: absolute; left: -5000px;" aria-hidden="true"><input type="text" name="b_2df44e8960266388bff165fa6_ee2f2a3737" tabindex="-1" value=""></div>
            <div class="form__container">
              <label for="mce-EMAIL" class="form__label">Email Address</label>
              <input type="email" value="" id="mce-EMAIL" class="form__input" name="EMAIL" placeholder="you@greatemail.com">
              <input type="submit" value="Subscribe" name="subscribe" class="form__submit">
            </div>
            <small>Note: I will <em>never</em> give out your email nor spam you – that ain't right.</small>
          </form>
          <!--End mc_embed_signup-->
        </section>
        <section class="spaceUp">
          ${postList(posts)}
        </section>
      </section>
    </div>
  </main>
`
