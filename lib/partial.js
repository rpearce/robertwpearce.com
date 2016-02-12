import { join } from 'path';
import { sourceDir } from '../config';
import { readFileString } from './utils';

export const readPartial = (filename) => {
  const filepath = join(sourceDir, 'partials', filename);
  return readFileString(filepath);
}
