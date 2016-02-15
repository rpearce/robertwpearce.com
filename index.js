import fsp from 'fs-promise';
import { outputDir } from './config';
import { readPosts, writePosts } from './lib/posts';
import { writeViews } from './lib/views';
import { writeCSS } from './lib/css';
import { copyImages } from './lib/images';

(async () => {
  try {
    await fsp.mkdir(outputDir);
    writeCSS();
    copyImages();
    const posts = await readPosts();
    writePosts(posts);
    writeViews(posts);
  } catch (err) {
    console.error(err);
  }
})();
