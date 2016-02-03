import fsp from 'fs-promise';
import hogan from 'hogan.js';
import walk from 'walkdir';
import { join, relative } from 'path';
import { layoutFile, outputDir, sourceDir } from './config';

(async () => {
  try {
    await fsp.mkdir(outputDir);

    const layout = await fsp.readFile(join(__dirname, sourceDir, layoutFile));
    const layoutTemplate = hogan.compile(layout.toString());

    const walkEmitter = walk(join(__dirname, sourceDir));
    walkEmitter.on('directory', async (path, stat) => {
      await fsp.mkdir(join(outputDir, relative(sourceDir, path)))
    });

    walkEmitter.on('file', async (path, stat) => {
      const relativePath = relative(sourceDir, path);
      if (!(relativePath.match(layoutFile))) {
        try {
          const view = await fsp.readFile(path);
          const viewTemplate = hogan.compile(view.toString());
          const rendered = viewTemplate.render({}, { layout: layoutTemplate });
          await fsp.writeFile(join(outputDir, relativePath), rendered, 'utf-8');
        } catch (err) {
          console.error(err);
        }
      }
    });
  } catch (err) {
    console.error(err);
  }
})();
