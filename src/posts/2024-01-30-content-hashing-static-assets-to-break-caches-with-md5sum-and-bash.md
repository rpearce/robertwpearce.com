---
author: "Robert Pearce"
authorTwitter: "@RobertWPearce"
desc: "Join me as I (over-)engineer implementing cache-busting for static assets on a simple website!"
keywords: "bash, md5, md5sum, content hashing, static assets, static site"
title: "Content hashing static assets to break caches with md5sum and bash"
updated: "2024-01-31T01:00:00Z"
---

## The Problem

When I make something for the web, I have plenty of "static assets": CSS files,
JS files, and images. In my HTML files, I reference their sources as you might
expect:

```html
<link rel="stylesheet" href="./styles.css" />
<script async src="./scripts.js"></script>
<img src="./cool-image.avif" />
```

But when a browser downloads these, it will note the source URLs and cache the
static assets, typically for a time set by the server via headers like
`Expires: <some date>` or `Cache-Control: public, max-age=15552000` (6 months).
This is exactly what we want the browser to do, but what happens if I change the
contents of the file?

Nothing! The last-cached content is served!

The browser caching is doing its job to help the user by not re-downloading the
same content, but if we have updated static asset content, we need to give the
browser a way to know that the content is different. Then it should go download
and use that updated asset.

One option, if I'm using a CDN, is to manually purge the assets. There are also
some additional headers, like `ETag` and `Last-Modified`, that can help hint to
a browser that it can keep its cache or not for an asset, but if you don't want
to mess with request headers and/or want to guarantee the browser gets the
latest version of an asset, you can provide a _different_ file name for each
version of an asset. Since the URL to the resource is different, the browser
should always go and try to download it.

Yes, frameworks like [Ruby on Rails](https://rubyonrails.org) and
[Phoenix](https://www.phoenixframework.org) will automatically do this sort of
thing for you, but we're exploring here and trying to keep things simple! (That
will remain to be seen... 😅)

Let's talk about how we can DIY (do it yourself).

## What our assets should look like when we're done

What we want to do is take a file like `styles.css`, get an [MD5 fingerprint
(checksum/digest/hash)](https://en.wikipedia.org/wiki/MD5) based on the file's
contents, and output something like `styles.78f7f2c2d416e59525938565dd6dd565.css`.
This way, if _anything_ in our file changes, we'll get a new hash and therefore
a new file name.

Given we have these files:

```
index.html
cool-image.avif
scripts.js
styles.css
```

and our `index.html` file contains this:

```html
<link rel="stylesheet" href="./styles.css" />
<script async src="./scripts.js"></script>
<img alt="" src="./cool-image.avif" />
```

then we should create a `dist/` directory with files that resemble these:

```
index.html
cool-image.dadb0e162005e9b241a13ca5f871e250.avif
scripts.9efef7ad3d06e7703c7563dbc1ed78a9.js
styles.78f7f2c2d416e59525938565dd6dd565.css
```

and our `index.html` file should have its assets' paths updated to resemble
these:

```html
<link rel="stylesheet" href="./styles.78f7f2c2d416e59525938565dd6dd565.css" />
<script async src="./scripts.9efef7ad3d06e7703c7563dbc1ed78a9.js"></script>
<img src="./cool-image.dadb0e162005e9b241a13ca5f871e250.avif" />
```

## Using md5sum to get file content hashes

If you haven't used `md5sum` before, go ahead and run `man md5sum` in your
terminal. There are some neat things you can use this for, like storing a list
of file checksums in a file, then detecting which files changed, having your
build system make decisions based on that, and avoiding costly project rebuilds
by only rebuilding files or directories and their dependencies that changed. But
we only need the top-level, most basic thing from `md5sum`: computing an MD5
message digest.

Let's say this is what our project folder looks like:

```
λ tree -a -L 1
.
├── .git
├── .gitignore
├── cool-image.avif
├── dist
├── index.html
├── scripts.js
└── styles.css
```

If I want to get an MD5 content hash for `styles.css`, I pass the filename to
`md5sm`:

```
λ md5sum styles.css
e6dd05b39c5fb97218130638c0a374de  styles.css
```

Sweet! If we want to query by a bunch of different file extensions, `md5sum` can
handle that:

```
λ md5sum *.{avif,css,js}
dadb0e162005e9b241a13ca5f871e250  cool-image.avif
e6dd05b39c5fb97218130638c0a374de  styles.css
78f7f2c2d416e59525938565dd6dd565  bingo.js
```

But if we want `md5sum` to ignore certain directories, find a bunch of different
file types, and maybe do so a bit more efficiently, we can lean on the `find`
tool. Run `man find` if you're unfamiliar with it or can't remember its syntax!

Let's run it with some options and then break down what we did:

```
λ find . \
  -type f \
  ! -path "./.git/*" \
  ! -path "./dist/*" \
  \( -iname "*.css" -o \
     -iname "*.js" -o \
     -iname "*.avif" -o \
     -iname "*.bmp" -o \
     -iname "*.gif" -o \
     -iname "*.heif" -o \
     -iname "*.jpeg" -o \
     -iname "*.jpg" -o \
     -iname "*.png" -o \
     -iname "*.svg" -o \
     -iname "*.webp" \
  \) \
  -exec md5sum '{}' +

e6dd05b39c5fb97218130638c0a374de  ./styles.css
dadb0e162005e9b241a13ca5f871e250  ./cool-image.avif
78f7f2c2d416e59525938565dd6dd565  ./bingo.js
```

Above, we told the find command to find all files in this directory, excluding
the `.git/` and `dist/` directories, where the file extension ends in one of a
handful of extensions of likely static assets, and then we tell it to execute
`md5sum` on each one. At the bottom, we see the results!

Next, we want to take that MD5 hash on the left and output a new file where the
filename has the hash just before the extension. For that, we're going to want
to start putting this into a `build` script.

## Starting our build script

In your terminal, run the following commands to create a build file with some
scaffolding, then change it to an executable file (don't copy the λ):

```
λ cat <<EOF > ./build
#!/usr/bin/env bash

set -o errexit
set -o errtrace
set -o nounset
set -eou pipefail

function main {
}

main
EOF

λ chmod +x ./build
```

Once you've done that open the file, and let's add our find function in there:

```bash
# ...

BUILD_DIR="./dist"

function get_asset_md5sums {
  find . \
    -type f \
    ! -path "./.git/*" \
    ! -path "${BUILD_DIR}/*" \
    \( -iname "*.css" -o \
       -iname "*.js" -o \
       -iname "*.avif" -o \
       -iname "*.bmp" -o \
       -iname "*.gif" -o \
       -iname "*.heif" -o \
       -iname "*.jpeg" -o \
       -iname "*.jpg" -o \
       -iname "*.png" -o \
       -iname "*.svg" -o \
       -iname "*.webp" \
    \) \
    -exec md5sum '{}' +
}

function main {
  get_asset_md5sums
}
```

If you then run that file via `./build`, you'll get back the same results as
before.

## Outputting hashed static asset file names

Update your `main` function with the following. We'll use code comments to
explain most of this part:

```bash
function main {
  # Recreate build dir
  rm -rf "${BUILD_DIR}" && mkdir -p "${BUILD_DIR}"

  # Create a bash array for holding # "file=file_with_sum"
  # pairs for use later. Yes, I know bash 4 has associative arrays.
  # E.g.: "styles.css=styles.78f7f2c2d416e59525938565dd6dd565.css"
  assets_array=()

  # Get all asset MD5 checksums, put them into an assets
  # array for later use, and write each file to a new
  # file with the checksum in the name.
  while read -r sum file; do
    file_name="${file%.*}" # Extract the file's name
    file_ext="${file##*.}" # Extract the file's extension
    file_with_sum="${file_name}.${sum}.${file_ext}" # Hashed file name

    # Append to the assets array
    assets_array+=( "${file}=${file_with_sum}" )

    # Write the file's contents to the build directory
    # at the new, hashed file name.
    cat "${file}" > "${BUILD_DIR}/${file_with_sum}"
  done < <(get_asset_md5sums)
}
```

If you're wondering about the `<(get_asset_md5sums)` part, it uses
[process substitution](https://www.gnu.org/software/bash/manual/bash.html#Process-Substitution)
to let us have access to the `assets_array` variable, which we wouldn't have
access to if we piped `get_asset_md5sums` to `while read ...`, for the `while`
loop would be ran in a [subshell environment](https://www.gnu.org/software/bash/manual/html_node/Command-Execution-Environment.html).
Instead, with process substitution, the result of that function is stored in a
named pipe/special temporary file (in `/dev/fd/` on my system), the file name is
passed, and then its contents are read and attached to the standard input by the
`<` input file descriptor. To sum this aside up, if we did `get_asset_md5sums | while read...`,
we'd get an `assets_array[@]: unbound variable` error, so we're using process
substitution to get around that.

If you run `./build` again, you won't see any terminal output, but you will see
a shiny new `./dist` folder with your files in it!

The next part is a little more involved, for we need to create new HTML files
that have updated values for asset source locations.

## Including the hashed file names in our HTML file(s)

Before we can copy, update, and output our HTML files (of which we only have one
in this example), we first need a way to find them! Add this below your
`get_asset_md5sums` function:

```bash
function get_html_files {
  find . \
    -type f \
    ! -path "./.git/*" \
    ! -path "${BUILD_DIR}/*" \
    -iname "*.html"
}
```

Next, at the bottom of your `main` function, add this code, and we'll use
comments to try to explain each piece in context:

```bash
# For each HTML file...
while read -r file; do
  # For each line in the current HTML file...
  while IFS='' read -r line; do
    line_updated="${line}"

    # For each "file=file_with_sum" pairing...
    for val in "${assets_array[@]}"; do
      file_name_original=$(echo "${val}" | cut -d "=" -f 1)
      file_name_summed=$(echo "${val}" | cut -d "=" -f 2)

      # If the current line has the original file name...
      if [[ "${line}" =~ ${file_name_original} ]]; then
        # ...then replace that file name with the hashed one
        line_updated=$(echo "${line}" | sed -E 's@'"${file_name_original}"'@'"${file_name_summed}"'@g')
        break
      fi
    done

    # Print the line
    echo "${line_updated}"

  # Pass the file in, then once done, redirect the
  # printed file lines to a new file in our build dir
  done < "${file}" > "${BUILD_DIR}/${file}"

# Pass the HTML files in via process substitution
done < <(get_html_files)
```

Once this code runs, your HTML files should be copied over to your `dist/`
directory, but the lines referencing your static assets should all be updated!

_This runs fast enough for my purposes, but if you have any performance tips or
explanation corrections, please email me._

## Building with GitHub Actions and deploying to GitHub Pages

Once you've got this building, you _could_ build it locally and push the `dist/`
folder up to your source control, but I want `./build` to run automatically, and
since I primarily use GitHub, I just want to deploy the `dist/` directory to
GitHub Pages.

To make this a reality, we can use Github Actions to deploy to GitHub Pages.
Create a `.github/workflows/main.yml` (the YAML file name can be whatever you
like) and add the following:

```yaml
name: CI

on:
  pull_request:
  push:

jobs:
  build:
    runs-on: ubuntu-latest
    permissions:
      contents: write
    steps:
      - name: Checkout repo under GH workspace
        uses: actions/checkout@v4

      - name: Run build script
        run: ./build

      - name: Deploy to gh-pages
        uses: peaceiris/actions-gh-pages@v3
        if: ${{ github.ref == 'refs/heads/main' }}
        with:
          github_token: ${{ secrets.GITHUB_TOKEN }}
          publish_dir: ./dist
```

After you've committed and merged everything into your `main` branch, go to your
project's "Settings" page, then click on "Pages" on the left, and set your
"Branch" to point to `gh-pages` and `/ (root)`. The page should tell you that
your site is live and give you a link and button to visit the site.

## Example project

Here is a silly project where everything in this blog post was implemented:
https://github.com/rpearce/gom-jabbar-bingo.

## Wrapping up

So... did we over-engineer our website? Maybe? But we're also avoiding static
asset caching issues by automating away a guaranteed way of cache-busting our
static assets, so that's something!

If you'd like to see more bash content or something else entirely, send me an
email!

* * *

Thanks for reading!<br />
— Robert
