import fsp from 'fs-promise';
import hogan from 'hogan.js';
import walk from 'walkdir';
import path from 'path';
import { layoutFilename, outputDir, sourceDir, viewsDir } from './config';

(async () => {
  try {
    const viewsPath = path.join(sourceDir, viewsDir);

    /*
     * Create the initial output directory
     */
    await fsp.mkdir(outputDir);

    /*
     * Read in and compile the layout file
     */
    const layoutPath = path.join(viewsPath, layoutFilename);
    const layout = await fsp.readFile(layoutPath);
    const layoutTemplate = hogan.compile(layout.toString());

    /*
     * Set up an even emitter for walking the source directory
     */
    const walkEmitter = walk(path.join(viewsPath));

    /*
     * When you come across a directory,
     * create the same one in the outputDir.
     */
    walkEmitter.on('directory', async (dirpath, stat) => {
      try {
        await fsp.mkdir(path.join(outputDir, path.relative(viewsPath, dirpath)));
      } catch (err) {
        console.error(err);
      }
    });

    /*
     * When you come across a file that isn't the layout file,
     * read it, compile it, and write the output to the outputDir.
     */
    walkEmitter.on('file', async (filepath, stat) => {
      const relativePath = path.relative(viewsPath, filepath);
      if (!(relativePath.match(layoutFilename))) {
        try {
          const view = await fsp.readFile(filepath);
          const viewTemplate = hogan.compile(view.toString());
          const rendered = viewTemplate.render({}, { layout: layoutTemplate });
          await fsp.writeFile(path.join(outputDir, relativePath), rendered, 'utf-8');
        } catch (err) {
          console.error(err);
        }
      }
    });
  } catch (err) {
    console.error(err);
  }
})();
