import { reviews } from './reviews';

export default {
  render() {
    const items = reviews.map(buildItem).join('');
    return `<ul class="list--bare">${items}</ul>`;
  }
};

const buildItem = (review) => {
  const { name, image, website } = review.author;
  return `<li class="review">
    <div class="review__author">
      <img class="review__authorImg" width="75" height="75" src="${image}" alt="Review author: ${name}" title="Review author: ${name}" />
      <small><a href="${website}">${name}</a></small>
    </div>
    <blockquote class="review__text">&quot;${review.body}&quot;</blockquote>
  </li>`;
}
