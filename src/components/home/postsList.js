export default {
  render(posts) {
    const items = posts.map(buildItem).join('');
    return `<ul class="list--bare list--vertIndex">${items}</ul>`;
  }
};

const buildItem = ({ metadata }) => {
  return `<li>
    <div><a href="${metadata.relativePath}">${metadata.title}</a></div>
    <small>${metadata.friendlyDate}</small>
  </li>`;
}
