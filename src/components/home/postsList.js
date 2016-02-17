import { posts } from '../../../posts';

export default {
  render() {
    const items = posts.sort(compareDate).map(buildItem).join('');
    return `<ul class="list--bare list--vertIndex">${items}</ul>`;
  }
};

const buildItem = ({ metadata }) => {
  return `<li>
    <div><a href="${metadata.relativePath}">${metadata.title}</a></div>
    <small>${metadata.friendlyDate}</small>
  </li>`;
}

const compareDate = (a, b) => {
  return new Date(b.metadata.date) - new Date(a.metadata.date);
}
