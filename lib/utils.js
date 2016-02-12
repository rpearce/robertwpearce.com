import fsp from 'fs-promise';

export const readFileString = async (filepath) => {
  try {
    const view = await fsp.readFile(filepath);
    return view.toString();
  } catch (err) {
    console.error(err);
  }
}
