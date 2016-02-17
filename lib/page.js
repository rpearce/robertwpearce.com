import fsp from 'fs-promise';
import path from 'path';
import walk from 'walkdir';
import { outputDir, sourceDir } from '../config';

import layout from '../src/components/layout';

export const writePage = async ({ component, metadata, data }) => {
  try {
    const body = component.render(data);
    const page = layout.render({ metadata, body });
    const outfile = path.join(outputDir, metadata.relativePath);
    fsp.writeFile(outfile, page, 'utf-8').then(null, (err) => { throw(err) });
  } catch (err) {
    console.error(err);
  }
}
