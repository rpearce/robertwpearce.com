import fsp from 'fs-promise';
import hogan from 'hogan.js';
import walk from 'walkdir';
import path from 'path';
import { outputDir, sourceDir } from './config';

const viewsDir = path.join(sourceDir, 'views');

(async () => {
  try {
    await fsp.mkdir(outputDir);
    writeViews();
    writePosts();
  } catch (err) {
    console.error(err);
  }
})();

const compileLayout = async (filename) => {
  try {
    const layoutPath = path.join(sourceDir, 'layouts', filename);
    const layoutFile = await fsp.readFile(layoutPath);
    return hogan.compile(layoutFile.toString());
  } catch (err) {
    console.error(err);
  }
}

const writeViews = async () => {
  const layout = await compileLayout('main.html');
  const walkEmitter = walk(viewsDir);

  /*
   * When you come across a directory,
   * create the same one in the outputDir.
   */
  walkEmitter.on('directory', async (dirpath, stat) => {
    try {
      await fsp.mkdir(path.join(outputDir, path.relative(viewsDir, dirpath)));
    } catch (err) {
      console.error(err);
    }
  });

  /*
   * When you come across an HTML file,
   * compile it and write it to the output folder.
   */
  walkEmitter.on('file', async (filepath, stat) => {
    const relativePath = path.relative(viewsDir, filepath);
    if (/\.html/.test(relativePath)) {
      try {
        const view = await fsp.readFile(filepath);
        const viewTemplate = hogan.compile(view.toString());
        const rendered = viewTemplate.render({}, { layout });
        console.log(layout);
        await fsp.writeFile(path.join(outputDir, relativePath), rendered, 'utf-8');
      } catch (err) {
        console.error(err);
      }
    }
  });
}

const writePosts = async () => {
  //const layout = await compileLayout('blog.html');

  //const walkEmitter = walk(path.join(viewsDir));
}
