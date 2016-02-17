import fse from 'fs-extra';
import path from 'path';
import { outputDir } from '../config';
import { posts } from '../posts';
import layoutComponent from '../src/components/layout';
import postBuilder from '../src/components/post/index';

/*
 * Import pages to rendered and output
 * to build directory, then include them
 * in pages array. Not super ideal, but hey.
 */
import homePage from '../src/components/home/index';

export const writePages = async () => {
  try {
    const pages = [
      homePage
    ];

    for (let page of pages) {
      let body = page.render();
      renderAndWrite({ page, body });
    }
  } catch(err) {
    console.error(err);
  }
}

export const writePosts = async () => {
  try {
    for (let post of posts) {
      let body = postBuilder.render({ post });
      renderAndWrite({ page: post, body });
    }
  } catch(err) {
    console.log(err);
  }
}

const renderAndWrite = ({ page, body }) => {
  try {
    const rendered = layoutComponent.render({ metadata: page.metadata, body });
    const outfile = path.join(outputDir, page.metadata.relativePath);
    fse.outputFile(outfile, rendered, (err) => { if (err !== null) { console.error(err) } });
  } catch(err) {
    console.error(err);
  }
}
