import fsp from 'fs-promise';
import { outputDir } from './config';
import { readPosts, writePosts } from './lib/posts';
import { writeViews } from './lib/views';
import { writeCSS } from './lib/css';

(async () => {
  try {
    await fsp.mkdir(outputDir);
    writeCSS();
    const posts = await readPosts();
    writePosts(posts);
    writeViews(posts);
  } catch (err) {
    console.error(err);
  }
})();
