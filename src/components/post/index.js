import marked from 'marked';
import prism from 'prismjs';
import '../../../node_modules/prismjs/components/prism-ruby';
import '../../../node_modules/prismjs/components/prism-elixir';
import '../../../node_modules/prismjs/components/prism-scss';
import '../../../node_modules/prismjs/components/prism-bash';
import nav from '../nav';

marked.setOptions({
  highlight(code, lang) {
    const grammar = lang !== undefined ? prism.languages[lang] : prism.languages.markup;
    return prism.highlight(code, grammar, lang);
  }
});

export default {
  render({ post }) {
    const { relativePath, title, friendlyDate, image } = post.metadata;
    const contentMarkup = marked(post.content);

    return `<div>
      ${nav.render()}
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
          ${contentMarkup}
        </div>
      </article>
    </div>`;
  }
}
