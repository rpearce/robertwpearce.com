import fsp from 'fs-promise';
import path from 'path';
import sass from 'node-sass';
import { outputDir, sourceDir, blogDirname } from '../config';

export const writeCSS = async () => {
  try {
    const outputCSSDir = path.join(outputDir, 'css');
    await fsp.mkdir(outputCSSDir);
    const cssDir = path.join(sourceDir, 'stylesheets');
    const entry = path.join(cssDir, 'app.scss');
    sass.render({
      file: entry,
      includePaths: [cssDir],
      outputStyle: 'compressed'
    }, async (err, result) => {
      try {
        if (err) { throw(err); }
        const outFile = path.join(outputCSSDir, 'app.css');
        await fsp.writeFile(outFile, result.css.toString(), 'utf-8');
      } catch (err) {
        console.error(err);
      }
    });
  } catch (err) {
    console.error(err);
  }
}
