import fs from 'fs';
import fsp from 'fs-promise';
import path from 'path';
import { outputDir } from './config';
import { readPosts, writePosts } from './lib/posts';
import { writeViews } from './lib/views';
import { writeCSS } from './lib/css';
import { copyImages } from './lib/images';

(async () => {
  try {
    await fsp.mkdir(outputDir);
    fs.createReadStream('CNAME').pipe(fs.createWriteStream(path.join(outputDir, 'CNAME')));
    writeCSS();
    copyImages();
    const posts = await readPosts();
    writePosts(posts);
    writeViews(posts);
  } catch (err) {
    console.error(err);
  }
})();
