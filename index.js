import fsp from 'fs-promise';
import getSlug from 'speakingurl';
import hogan from 'hogan.js';
import parseMD from 'parse-md';
import path from 'path';
import sass from 'node-sass';
import walk from 'walkdir';
import { outputDir, sourceDir } from './config';

(async () => {
  try {
    await fsp.mkdir(outputDir);
    writeViews();
    writeBlogPosts();
    writeCSS();
  } catch (err) {
    console.error(err);
  }
})();

const compileLayout = async (filename) => {
  try {
    const layoutPath = path.join(sourceDir, 'layouts', filename);
    const layoutFile = await fsp.readFile(layoutPath);
    return layoutFile.toString();
  } catch (err) {
    console.error(err);
  }
}

const writeViews = async () => {
  try {
    const viewsDir = path.join(sourceDir, 'views');

    const superLayout = await compileLayout('layout.html');
    const layout = await compileLayout('main.html');

    const walkEmitter = walk(viewsDir);

    /*
     * When you come across a directory,
     * create the same one in the outputDir.
     */
    walkEmitter.on('directory', async (dirpath, stat) => {
      try {
        await fsp.mkdir(path.join(outputDir, path.relative(viewsDir, dirpath)));
      } catch (err) {
        console.error(err);
      }
    });

    /*
     * When you come across an HTML file,
     * compile it and write it to the output folder.
     */
    walkEmitter.on('file', async (filepath, stat) => {
      const relativePath = path.relative(viewsDir, filepath);
      if (/\.html/.test(relativePath)) {
        try {
          const view = await fsp.readFile(filepath);
          const viewTemplate = hogan.compile(view.toString());
          const rendered = viewTemplate.render({}, { layout, superLayout });
          await fsp.writeFile(path.join(outputDir, relativePath), rendered, 'utf-8');
        } catch (err) {
          console.error(err);
        }
      }
    });
  } catch (err) {
    console.error(err);
  }
}

const writeBlogPosts = async () => {
  try {
    await fsp.mkdir(path.join(outputDir, 'blog'));
    const blogDir = path.join(sourceDir, 'blog');

    const superLayout = await compileLayout('layout.html');
    const layout = await compileLayout('blog.html');

    const walkEmitter = walk(path.join(blogDir));

    /*
     * When you come across a blog post,
     * compile it and write it to the output folder.
     */
    walkEmitter.on('file', async (filepath, stat) => {
      const relativePath = path.relative(blogDir, filepath);
      if (/\.md|\.markdown/.test(relativePath)) {
        try {
          const mdContents = await fsp.readFile(filepath);
          const { metadata, content } = parseMD(mdContents.toString());
          const templateString = `{{<layout}}{{$title}}${metadata.title}{{/title}}{{$description}}${metadata.description}{{/description}}{{$content}}${content}{{/content}}{{/layout}}`;
          const viewTemplate = hogan.compile(templateString);
          const rendered = viewTemplate.render({}, { layout, superLayout });
          const slug = getSlug(metadata.title);
          await fsp.writeFile(path.join(outputDir, 'blog', `${slug}.html`), rendered, 'utf-8');
        } catch (err) {
          console.error(err);
        }
      }
    });
  } catch (err) {
    console.error(err);
  }
}

const writeCSS = async () => {
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
