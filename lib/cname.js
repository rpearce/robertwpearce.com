import fse from 'fs-extra';
import { outputDir } from '../config';

export const copyCNAME = () => {
  fse.copy('CNAME', `${outputDir}/CNAME`, (err) => {
    if (err) {
      console.error(err);
    }
  });
}
