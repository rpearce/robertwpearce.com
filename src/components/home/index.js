import postsList from './postsList';
import reviewsList from './reviewsList';

export default {
  metadata: {
    title: 'Robert Pearce | Freelance Software Developer',
    description: 'Need to develop web-based software? You\'ve come to the right place.',
    image: null,
    relativePath: '/index.html',
    type: 'website'
  },

  render() {
    return `<main>
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
            </ul>
          </div>
        </header>
        <section class="spaceUp">
          <header>
            <h2 class="heading--bordered">Recommendations</h2>
          </header>
          ${reviewsList.render()}
        </section>
        <section class="spaceUp">
          <header>
            <h2 class="heading--bordered">Blog Posts</h2>
          </header>
          ${postsList.render()}
        </section>
      </div>
    </main>`;
  }
};
