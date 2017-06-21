'use strict'

module.exports = () => `
  <section class="section--padded bg--gray">
    <div class="layout--constrained">
      <h2>Get Updates</h2>
      <!-- Begin MailChimp Signup Form -->
      <form action="//robertwpearce.us13.list-manage.com/subscribe/post?u=2df44e8960266388bff165fa6&amp;id=ee2f2a3737" method="post" name="mc-embedded-subscribe-form" target="_blank" novalidate class="form">
        <div id="mce-responses">
          <div class="response" id="mce-error-response" style="display:none"></div>
          <div class="response" id="mce-success-response" style="display:none"></div>
        </div>
        <div style="position: absolute; left: -5000px;" aria-hidden="true"><input type="text" name="b_2df44e8960266388bff165fa6_ee2f2a3737" tabindex="-1" value=""></div>
        <div class="form__container">
          <label for="mce-EMAIL" class="form__label">Email Address</label>
          <input type="email" value="" id="mce-EMAIL" class="form__input" name="EMAIL" placeholder="you@greatemail.com">
          <input type="submit" value="Subscribe" name="subscribe" class="form__submit">
        </div>
        <small>Note: I will <em>never</em> give out your email nor spam you. Unsubscribe at any time.</small>
      </form>
      <!--End mc_embed_signup-->
    </div>
  </section>
`
