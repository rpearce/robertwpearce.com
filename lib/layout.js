import fsp from 'fs-promise';
import path from 'path';
import { sourceDir } from '../config';

export const compileLayout = async (filename) => {
  try {
    const layoutPath = path.join(sourceDir, 'layouts', filename);
    const layoutFile = await fsp.readFile(layoutPath);
    return layoutFile.toString();
  } catch (err) {
    console.error(err);
  }
}
