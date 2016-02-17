import fsp from 'fs-promise';
import { outputDir } from './config';

import { copyCNAME } from './lib/cname';
import { copyImages } from './lib/images';
import { writeCSS } from './lib/css';
import { writePages } from './lib/fileUtils';
import { writePosts } from './lib/fileUtils';

(async () => {
  try {
    await fsp.mkdir(outputDir);
    copyCNAME();
    copyImages();
    writeCSS();
    writePages();
    writePosts();
  } catch (err) {
    console.error(err);
  }
})();
