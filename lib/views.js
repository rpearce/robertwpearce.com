import fsp from 'fs-promise';
import hogan from 'hogan.js';
import path from 'path';
import walk from 'walkdir';
import { outputDir, sourceDir } from '../config';
import { readFileString } from './utils';
import { readLayout } from './layout';
import { readPartial } from './partial';
import { getPostMeta } from './posts';

export const writeViews = async (posts) => {
  const viewsDir = path.join(sourceDir, 'views');
  try {
    const superLayout = await readLayout('layout.html');
    const layout = await readLayout('main.html');
    const walkEmitter = walk(viewsDir);

    walkEmitter.on('directory', async (dirpath, stat) => {
      try {
        await fsp.mkdir(path.join(outputDir, path.relative(viewsDir, dirpath)));
      } catch (err) {
        if (err.code !== 'EEXIST') { console.error(err); }
      }
    });

    walkEmitter.on('file', async (filepath, stat) => {
      const relativePath = path.relative(viewsDir, filepath);
      if (/\.html/.test(relativePath)) {
        try {
          const viewString = await readFileString(filepath);
          const view = hogan.compile(viewString);
          const nav = await readPartial('nav.html');
          const data = {
            posts: posts.reverse().map(getPostMeta),
            relativePath: relativePath === 'index.html' ? '' : relativePath
          };
          const rendered = view.render(data, { layout, superLayout, nav });
          await fsp.writeFile(path.join(outputDir, relativePath), rendered, 'utf-8');
        } catch (err) {
          console.error(err);
        }
      }
    });
  } catch (err) {
    console.error(err);
  }
}
