import { join } from 'path';
import { exec } from 'child_process';
import fsp from 'fs-promise';
import { outputDir, sourceDir } from '../config';

export const copyImages = () => {
  const imagesDir = join(sourceDir, 'images');

  exec(`cp -r ${imagesDir} ${outputDir}`, (err) => {
    if (err !== null) {
      console.error(error);
    };
  });
}
