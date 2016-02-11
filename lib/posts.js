import fsp from 'fs-promise';
import getSlug from 'speakingurl';
import hogan from 'hogan.js';
import parseMD from 'parse-md';
import path from 'path';
import walk from 'walkdir';
import { outputDir, sourceDir, blogDirname } from '../config';
import { compileLayout } from './layout';

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
            let url = `/${blogDirname}/${slug}.html`;
            parsed.metadata = Object.assign(parsed.metadata, { slug, url });
            let { metadata, content } = parsed;
            posts.push({ metadata, content });
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
    const superLayout = await compileLayout('layout.html');
    const layout = await compileLayout('blog.html');
    for (let { metadata, content } of posts) {
      let templateString = `{{<layout}}{{$title}}${metadata.title}{{/title}}{{$description}}${metadata.description}{{/description}}{{$content}}${content}{{/content}}{{/layout}}`;
      let template = hogan.compile(templateString);
      let rendered = template.render({}, { layout, superLayout });
      await fsp.writeFile(path.join(outputDir, blogDirname, `${metadata.slug}.html`), rendered, 'utf-8');
    }
  } catch (err) {
    console.error(err);
  }
}
