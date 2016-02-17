import fsp from 'fs-promise';
import getSlug from 'speakingurl';
import parseMD from 'parse-md';
import path from 'path';
import walk from 'walkdir';
import { outputDir, sourceDir, blogDirname } from '../config';

import layout from '../src/components/layout';
import postTemplate from '../src/components/post/index';

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
            let friendlyDate = getFriendlyDate(parsed.metadata.date);
            let relativePath = `/${blogDirname}/${slug}.html`;
            parsed.metadata = Object.assign(parsed.metadata, { relativePath, friendlyDate, type: 'article' });
            posts.push(parsed);
          }
        }
        resolve(posts);
      } catch (err) {
        reject(err);
      }
    });
  });
}

const getFriendlyDate = (date) => {
  const months = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'June', 'July', 'Aug', 'Sept', 'Oct', 'Nov', 'Dec'];
  const dateObj = new Date(date);
  const month = months[dateObj.getUTCMonth()];
  const day = dateObj.getUTCDate();
  const year = dateObj.getUTCFullYear();
  return `${month} ${day}, ${year}`;
}
