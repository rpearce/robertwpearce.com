---
author: "Robert Pearce"
authorTwitter: "@RobertWPearce"
desc: "A full walkthrough for getting set up to create static sites using nix and hakyll"
keywords: "hakyll, nix, hakyll-nix-template, haskell, static site generators, functional programming, programming"
title: "The hakyll-nix-template Tutorial"
---

If you're looking to use [hakyll](https://jaspervdj.be/hakyll) with
[nix](https://nixos.org) to build static sites, this reference article was made
for you!

We will be working with the
[hakyll-nix-template](https://github.com/rpearce/hakyll-nix-template/), so go
ahead and pull that up in a new browser tab. Its README also contains info on
all the features that are provided.

## Overview

* [Prerequisites](#prerequisites)
* [Copying the template](#copying-the-template)
* [Building the project](#building-the-project)
* [Getting into the haskell and nix dev environment](#getting-into-the-haskell-and-nix-dev-environment)
* [Personalizing the project build](#personalizing-the-project-build)
* [Adding your first post](#adding-your-first-post)
* [Working with page metadata](#working-with-page-metadata)
* [Determining what static files are copied over](#determining-what-static-files-are-copied-over)
* [Understanding the GitHub action workflow](#understanding-the-github-action-workflow)
* [Enabling GitHub Pages](#enabling-github-pages)
* [Deploying to your domain](#deploying-to-your-domain)
* [TODOs for hakyll-nix-template](#todos-for-hakyll-nix-template)
  * [TODO: Caching and hashing](#todo-caching-and-hashing)
  * [TODO: Use pygments for syntax highlighting](#todo-use-pygments-for-syntax-highlighting)
* [Other hakyll posts](#other-hakyll-posts)

## Prerequisites

If you don't have [nix](https://nixos.org), follow [the nix installation
instructions](https://nixos.org/download.html).

Once you have nix installed, follow the [nix flakes setup
instructions](https://nixos.wiki/wiki/Flakes), and then I highly recommend
installing [cachix](https://www.cachix.org), as well.

If it helps, here is [my `install_nix` bash
function](https://github.com/rpearce/dotfiles/blob/809e8fc298291c9819d4d2ffcf1d99b74a3931fe/install#L184-L217),
and here is my [`${XDG_CONFIG_HOME}/nix/nix.conf`
file](https://github.com/rpearce/dotfiles/blob/main/conf/.nix.conf) (note: on
macOS, this will likely be `~/.config/nix/nix.conf`). Feel free
to copy the conf file, and just remove `https://rpearce.cachix.org` from
`substituters` and `rpearce.cachix.org-1:...=` from the `trusted-public-keys`
(or replace with your own cache from cachix!).

While you're at it, we aren't using [devenv.sh](https://devenv.sh) nor
[nix-direnv](https://github.com/nix-community/nix-direnv) in this example, but
you should check them out later, too.

## Copying the template

From the [hakyll-nix-template](https://github.com/rpearce/hakyll-nix-template)
page, click "Use this template" and then select "Create a new repository" from
the popover menu.

<img
  alt="GitHub's 'Use this template' menu"
  decoding="async"
  height="246"
  loading="lazy"
  src="./images/hnt-copy.webp"
  width="422"
/>

Next, create a new repository from the template, filling in the details you want
for the repo.

<img
  alt="GitHub form for creating a new repository from a template"
  decoding="async"
  height="600"
  loading="lazy"
  src="./images/hnt-create.webp"
  width="801"
/>

After creating the repository, click the "&lt;&gt; Code" button, then choose
your method of cloning the repository.

<img
  alt="GitHub's code cloning menu"
  decoding="async"
  height="500"
  loading="lazy"
  src="./images/hnt-clone.webp"
  width="547"
/>

Once you've chosen your preferred cloning command and ran that in your terminal,
`cd` into the directory.

<img
  alt="Terminal that has cloned the repository and cd'd into the directory"
  decoding="async"
  height="520"
  loading="lazy"
  src="./images/hnt-clone-result.webp"
  width="968"
/>

Alright! We're ready to build and personalize our project.

## Building the project

Run `nix build`, answer any substituters trust prompts, and then go do something
else for a while. The first run takes a while, and how long it takes depends on
connection speed, processing speed, and — most importantly — what caches you
have set up in `nix.conf` (and/or `flake.nix`).

Once that is all done, you'll have a brand new `result/` directory available
that is a symlink to `/nix/store/<HASH>-website/`. For this blog, it looks like
this:

```default
result/
└── dist/
  ├── CNAME
  ├── _config.yml
  ├── announcing-react-medium-image-zoom-v4.html
  ├── asynchronously-loading-scripts.html
  ├── atom.xml
  ├── be-better.html
  ├── behaviour-your-team.html
  ├── berlin.html
  ├── build-your-team-an-accessible-shareable-component-library.html
  ├── catch-low-hanging-accessibility-fruit-with-axe-core.html
  ├── chief.html
  ├── css
  │   ├── article.css
  │   ├── default.css
  │   └── home.css
  ├── delegate-dont-dump.html
  ├── ...
```

This is your static output! While you could run `cd result/dist` and either `npx
serve .` or `python -m SimpleHTTPServer`, let's do this the
`hakyll-nix-template` way:

```default
λ nix run . watch
Listening on http://127.0.0.1:8000
Initialising...
  Creating store...
  Creating provider...
  Running rules...
Checking for out-of-date items
Compiling
Success
```

Lovely! If we navigate to http://127.0.0.1:8000, we'll see the default webpage
included in the project.

## Getting into the haskell and nix dev environment

In a new terminal pane or window, run `nix develop` (note: this may take a
while the first time):

```default
λ nix develop
[hakyll-nix]λ
```

When you have `[hakyll-nix]λ ` as your prompt, you know that you're in a nix
shell. This comes preloaded with _most_ of your existing CLI tools, plus
`cabal`, `ghc`, `haskell-language-server`, and `hlint`. If you want it to be
exactly your environment plus the nix develop shell, check out
[nix-direnv](https://github.com/nix-community/nix-direnv).

At this point, if you're using Vim, for example, you can run `vim .` and open
the project up _with access to the aforementioned tools_.

Now, it's time to customize the project for you.

## Personalizing the project build

First, go back to your window where you can `nix run . watch` and cancel that;
e.g., press `ctrl + c`.

Next, using your editor, open `ssg/src/Main.hs`, and read over the
`PERSONALIZATION` section near the top:

```haskell
--------------------------------------------------------------------------------
-- PERSONALIZATION

mySiteName :: String
mySiteName = "My Site Name"

mySiteRoot :: String
mySiteRoot = "https://my-site.com"

myFeedTitle :: String
myFeedTitle = "My Site"

myFeedDescription :: String
myFeedDescription = "My Site Description"

myFeedAuthorName :: String
myFeedAuthorName = "My Name"

myFeedAuthorEmail :: String
myFeedAuthorEmail = "me@myemail.com"

myFeedRoot :: String
myFeedRoot = mySiteRoot
```

This area contains all the high level, site-based customization text and root
URLs for you to update. Go ahead and do that.

Below this area, you'll find the `CONFIG` section:

```haskell
-- Default configuration: https://github.com/jaspervdj/hakyll/blob/cd74877d41f41c4fba27768f84255e797748a31a/lib/Hakyll/Core/Configuration.hs#L101-L125
config :: Configuration
config =
  defaultConfiguration
    { destinationDirectory = "dist"
    , ignoreFile = ignoreFile'
    , previewHost = "127.0.0.1"
    , previewPort = 8000
    , providerDirectory = "src"
    , storeDirectory = "ssg/_cache"
    , tmpDirectory = "ssg/_tmp"
    }
  where
    ignoreFile' path
      | "."    `isPrefixOf` fileName = False
      | "#"    `isPrefixOf` fileName = True
      | "~"    `isSuffixOf` fileName = True
      | ".swp" `isSuffixOf` fileName = True
      | otherwise = False
      where
        fileName = takeFileName path
```

This section specifically deals with your hakyll config. If you want to change
the development server port, host, content, source directory, what files are or
aren't ignored, and some caching things, then you can do so here.

The rest of the file is all related to hakyll and the build, so if you know
hakyll already, this should feel familiar, and feel free to customize it
however you like.

Do note that any changes you make inside of `ssg/` means you'll need to turn
your dev server off and on again.

## Adding your first post

Now that we've customized our config, turn the dev server back on with `nix
run . watch`. It's time to add our first post!

Navigate to the `src/posts/` folder and add a new markdown file with this naming
format:

```default
2023-02-10-my-real-post.md
```

As you can see from the other posts already in this directory, we have post
metadata (a.k.a. front-matter) and then the post content follows that. For
example:

```markdown
---
author: "Robert Pearce"
authorTwitter: "@RobertWPearce"
desc: "Welcome to the fun, probably over-engineered world of nix and haskell to make a website"
image: "./images/some-image.webp"
keywords: "hakyll, nix, haskell, static site generator"
lang: "en"
title: "Today, I used hakyll-nix-template"
---

Hello, world! I am here!
```

...but customize this with your own content.

Save the file and watch your dev server reload and pick it up! If you refresh
your browser, you should now see your post on the index page.

## Working with page metadata

The `author`, `desc`, `title`, and other meta fields from the prior section are
all completely customizable by you! These are fields that you can change,
remove, or add more of, and they are used in your HTML templates in the
`src/templates/` folder.

If you open `src/templates/post.html`, you'll see something like this:

```html
<main>
  <article>
    <header>
      <h1>
        <a href=".$url$">$title$</a>
      </h1>
      <div>
        <small>$date$</small>
        $if(updated)$
        <small>(updated: $updated$)</small>
        $endif$
      </div>
    </header>
    <section>
      $body$
    </section>
  </article>
</main>
```

This is all a part of hakyll, but I'll cover some of this here to make it
easier to understand all in one place.

See `$title$`? That comes from our post metadata, and `updated` looks like it's
an optional field from our metadata, but where does `$date$` come from? Or
`$body$`?

In `ssg/src/Main.hs`, you'll see `postCtx`:

```haskell
postCtx :: Context String
postCtx =
  constField "root" mySiteRoot
    <> constField "siteName" mySiteName
    <> dateField "date" "%Y-%m-%d"
    <> defaultContext
```

This is a post context that gets built up and supplied to the template. Hakyll
has [a special `dateField` helper](https://github.com/jaspervdj/hakyll/blob/909e1b3a89b5b3ba5f64840d23ada9b3ac393404/lib/Hakyll/Web/Template/Context.hs#L295-L343)
that parses a date from your post filename if it begins with a date. It also has
[`defaultContext`](https://github.com/jaspervdj/hakyll/blob/909e1b3a89b5b3ba5f64840d23ada9b3ac393404/lib/Hakyll/Web/Template/Context.hs#L231-L249)
which handles things like your post/web page's body content.

What is significant about this example is that this is a place where you can
pass in values at a global level; note that `constField` is including some of
the personalization fields you filled out earlier. Passing those in the right
context gives your templates access to them.

You can read more on this from jaspervdj, themself:
https://jaspervdj.be/hakyll/tutorials/04-compilers.html

Before we wrap this section up, you should know that you can also add as many
templates as you like, as well, and reference them in other templates using this
format:

```html
<!-- Inside templates/post.html...  -->
<section class="section-subscribe">
  $partial("templates/subscribe.html")$
</section>
```

## Determining what static files are copied over

You will inevitably want to copy static files from your source code into your
outputted build, and this is easily with hakyll's `copyFileCompiler` in
`ssg/src/Main.hs`, just inside the `main` function.

```haskell
main :: IO ()
main = hakyllWith config $ do
  forM_
    [ "CNAME"
    , "favicon.ico"
    , "robots.txt"
    , "_config.yml"
    , "images/*"
    , "js/*"
    , "fonts/*"
    ]
    $ \f -> match f $ do
      route idRoute
      compile copyFileCompiler
```

Each file or folder glob here exists inside the `src/` directory. If you have
something you want copied over to the build, this is the place to do it.

If you find you need to ignore a certain file or extension, consult the
`ignoreFile'` function in the `config` and add your problematic file, prefix, or
extension to the guard. For example, my macOS likes to add `.DS_Store`
everywhere, so I did this:

```haskell
ignoreFile' path
  | ".DS_Store" == fileName = True -- this line
  | "."    `isPrefixOf` fileName = False
  | "#"    `isPrefixOf` fileName = True
  | -- ...
```

## Understanding the GitHub action workflow

There GitHub action workflow can be found in `.github/workflows/main.yml`. There
are two jobs here: `build-nix` and `deploy`, and `deploy` only runs on the
`main` branch.

### The `build-nix` job

This is the main job, and it does four things:

1. Install nix
1. Setup the build to run with cachix
1. Run `nix-build`
1. Temporarily upload the result of `nix-build` for use later (your website
   output)

### The `deploy` job

When code is pushed to the `main` branch, the `deploy` job will:

1. Run the `build-nix` job
1. Download the temporarily uploaded website output
1. If the prior step succeeds, it will checkout the `gh-pages` branch and
   deploy your code to that branch

### Adding your `CACHIX_AUTH_TOKEN`

You may have noticed a `{{ secrets.CACHIX_AUTH_TOKEN }}` used in this file. Here
are the steps to setting this up:

1. Follow the [cachix getting started
   guide](https://docs.cachix.org/getting-started), and get an auth token that
   is explicitly to be used for your GitHub workflow.
1. On your project GitHub page, click the `Settings` tab, then click on `Secrets
   and Variables`, then `Actions`, and add a repository secret called
   `CACHIX_AUTH_TOKEN` where you set that variable. At present, a direct link to
   this is https://github.com/youruser/yoursite.com/settings/secrets/actions

## Enabling GitHub Pages

While you're in the `Settings` tab, go to the `Pages` page, enable GitHub Pages,
set the `Source` to `Deploy from a branch`, set that branch to `gh-pages`, and
make sure the directory for that branch is `/ (root)`.

<img
  alt="The GitHub Pages setup for this website"
  decoding="async"
  height="517"
  loading="lazy"
  src="./images/hnt-gh-pages.webp"
  width="600"
/>

## Deploying to your domain

Follow the [GitHub Pages custom domain guide](https://docs.github.com/en/pages/configuring-a-custom-domain-for-your-github-pages-site/managing-a-custom-domain-for-your-github-pages-site)
for heaps of info on how to deploy your site to your web domain.

## TODOs for hakyll-nix-template

### TODO: Caching and hashing

When a CSS or JS file changes, we need a way to break browser caches to ensure
they get the latest version. The way to do this is to generate a hash of that
file's contents, generate a file with that content hash in the filename when
building, and then make sure any output that references that CSS or JS file
reflects this updated filename, as well.

I have no idea how to do this yet, but I'll figure it out!

### TODO: Use pygments for syntax highlighting

See [Tony Zorman's post on pygmentising hakyll](https://tony-zorman.com/posts/2023-01-21-pygmentising-hakyll.html)
for details on some issues with the [skylighting
library](https://hackage.haskell.org/package/skylighting). I'll likely follow
this post in order to switch up the syntax highlighting to something better, or
at least allow people to work with whatever they want.

## Other hakyll posts

* [Pt. 1 – Setup & Initial Customization](/hakyll-pt-1-setup-and-initial-customization.html)
* [Pt. 2 – Generating a Sitemap XML File](/hakyll-pt-2-generating-a-sitemap-xml-file.html)
* [Pt. 3 – Generating RSS and Atom XML Feeds](/hakyll-pt-3-generating-rss-and-atom-xml-feeds.html)
* [Pt. 4 – Copying Static Files For Your Build](/hakyll-pt-4-copying-static-files-for-your-build.html)
* [Pt. 5 – Generating Custom Post Filenames From a Title Slug](/hakyll-pt-5-generating-custom-post-filenames-from-a-title-slug.html)
* [Pt. 6 – Pure Builds With Nix](/hakyll-pt-6-pure-builds-with-nix.html)
