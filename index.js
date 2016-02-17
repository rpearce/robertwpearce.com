import fs from 'fs';
import fsp from 'fs-promise';
import path from 'path';
import { outputDir, blogDirname } from './config';
import { readPosts } from './lib/posts';
import { writePage } from './lib/page';
import { writeCSS } from './lib/css';
import { copyImages } from './lib/images';
import { exec } from 'child_process';

import homeComponent from './src/components/home/index';
import postComponent from './src/components/post/index';

(async () => {
  try {
    await fsp.mkdir(outputDir);
    exec(`cp CNAME ${outputDir}`, (err) => { if (err !== null) { throw(err) } });
    writeCSS();
    copyImages();

    await fsp.mkdir(path.join(outputDir, blogDirname));

    const posts = await readPosts();
    posts.reverse();
    const pages = [
      { component: homeComponent, metadata: homeComponent.metadata, data: { posts } }
    ];

    for (let post of posts) {
      pages.push({ component: postComponent, metadata: post.metadata, data: { post } });
    }

    for (let page of pages) {
      writePage(page);
    }

  } catch (err) {
    console.error(err);
  }
})();
