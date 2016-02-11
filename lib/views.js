import fsp from 'fs-promise';
import hogan from 'hogan.js';
import path from 'path';
import walk from 'walkdir';
import { outputDir, sourceDir } from '../config';
import { compileLayout } from './layout';

export const writeViews = async (posts) => {
  try {
    const viewsDir = path.join(sourceDir, 'views');
    const superLayout = await compileLayout('layout.html');
    const layout = await compileLayout('main.html');
    const walkEmitter = walk(viewsDir);
    walkEmitter.on('directory', async (dirpath, stat) => {
      try {
        await fsp.mkdir(path.join(outputDir, path.relative(viewsDir, dirpath)));
      } catch (err) {
        console.error(err);
      }
    });

    walkEmitter.on('file', async (filepath, stat) => {
      try {
        const relativePath = path.relative(viewsDir, filepath);
        if (/\.html/.test(relativePath)) {
          const view = await fsp.readFile(filepath);
          const viewTemplate = hogan.compile(view.toString());
          const data = {};
          if (relativePath === 'index.html') {
            data.posts = posts.map(post => post.metadata);
          }
          const rendered = viewTemplate.render(data, { layout, superLayout });
          await fsp.writeFile(path.join(outputDir, relativePath), rendered, 'utf-8');
        }
      } catch (err) {
        console.error(err);
      }
    });
  } catch (err) {
    console.error(err);
  }
}
