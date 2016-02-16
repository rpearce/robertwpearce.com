import { join } from 'path';
import { sourceDir } from '../config';
import { readFileString } from './utils';

export const readLayout = (filename) => {
  const filepath = join(sourceDir, 'layouts', filename);
  return readFileString(filepath);
}