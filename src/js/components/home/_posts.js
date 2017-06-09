'use strict'

const buildItem = ({ metadata }) => {
  return `<li>
    <div><a href="${metadata.relativePath}">${metadata.title}</a></div>
    <small>${metadata.friendlyDate}</small>
  </li>`;
}

const compareDate = (a, b) => {
  return new Date(b.metadata.date) - new Date(a.metadata.date);
}

module.exports = posts => `
  <ul class="list--bare list--vertIndex">
    ${posts.sort(compareDate).map(buildItem).join('')}
  </ul>
`
