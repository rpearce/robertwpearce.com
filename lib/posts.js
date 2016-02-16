import fsp from 'fs-promise';
import getSlug from 'speakingurl';
import hogan from 'hogan.js';
import parseMD from 'parse-md';
import path from 'path';
import walk from 'walkdir';
import { outputDir, sourceDir, blogDirname } from '../config';
import { readLayout } from './layout';
import { readPartial } from './partial';
import marked from 'marked';
import prism from 'prismjs';
import '../node_modules/prismjs/components/prism-ruby';
import '../node_modules/prismjs/components/prism-elixir';
import '../node_modules/prismjs/components/prism-scss';
import '../node_modules/prismjs/components/prism-bash';


marked.setOptions({
  highlight(code, lang) {
    const grammar = lang !== undefined ? prism.languages[lang] : prism.languages.markup;
    return prism.highlight(code, grammar, lang);
  }
});

export const readPosts = () => {
  return new Promise((resolve, reject) => {

    const filepaths = [];
    const blogSrcDir = path.join(sourceDir, blogDirname);

    const walkEmitter = walk(blogSrcDir);

    walkEmitter.on('file', (filepath, stat) => filepaths.push(filepath));
    walkEmitter.on('end',  async () => {
      try {
        const posts = [];
        for (let filepath of filepaths) {
          let relativePath = path.relative(blogSrcDir, filepath);
          if (/\.md|\.markdown/.test(relativePath)) {
            let mdContents = await fsp.readFile(filepath);
            let parsed = parseMD(mdContents.toString());
            let slug = getSlug(parsed.metadata.title);
            let relativePath = `/${blogDirname}/${slug}.html`;
            parsed.metadata = Object.assign(Object.create(parsed.metadata), { slug, relativePath });
            let { metadata, content } = parsed;
            posts.push({ metadata, content: marked(content) });
          }
        }
        resolve(posts);
      } catch (err) {
        reject(err);
      }
    });
  });
}

export const writePosts = async (posts) => {
  try {
    await fsp.mkdir(path.join(outputDir, blogDirname));
    const superLayout = await readLayout('layout.html');
    const layout = await readLayout('post.html');
    const nav = await readPartial('nav.html');
    for (let { metadata, content } of posts) {
      let friendlyDate = getFriendlyDate(metadata.date);
      let decoratedMeta = Object.assign(Object.create(metadata), { friendlyDate });
      let templateString = buildTemplateString({ metadata: decoratedMeta, content });
      let template = hogan.compile(templateString);
      let rendered = template.render({ relativePath: metadata.relativePath }, { layout, superLayout, nav });
      let outfile = path.join(outputDir, blogDirname, `${metadata.slug}.html`);
      fsp.writeFile(outfile, rendered, 'utf-8').then(null, (err) => { throw(err) });
    }
  } catch (err) {
    if (err.code !== 'EEXIST') { console.error(err); }
  }
}

export const getPostMeta = ({ metadata, _ }) => {
  const friendlyDate = getFriendlyDate(metadata.date);
  return Object.assign(Object.create(metadata), { friendlyDate });
}

const buildTemplateString = ({ metadata, content }) => {
   return `{{<layout}}
      {{$title}}${metadata.title}{{/title}}
      {{$description}}${metadata.description}{{/description}}
      {{$friendlyDate}}${metadata.friendlyDate}{{/friendlyDate}}
      {{$bgImage}}${metadata.bgImage}{{/bgImage}}
      {{$relativePath}}${metadata.relativePath}{{/relativePath}}
      {{$content}}${content}{{/content}}
   {{/layout}}`;
}

const getFriendlyDate = (date) => {
  const months = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'June', 'July', 'Aug', 'Sept', 'Oct', 'Nov', 'Dec'];
  const dateObj = new Date(date);
  const month = months[dateObj.getUTCMonth()];
  const day = dateObj.getUTCDate();
  const year = dateObj.getUTCFullYear();
  return `${month} ${day}, ${year}`;
}
