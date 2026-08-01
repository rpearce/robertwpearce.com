---
author: "Robert Pearce"
authorTwitter: "@RobertWPearce"
desc: "A full walkthrough for getting set up to create static sites using nix and hakyll"
keywords: "hakyll, nix, hakyll-nix-template, haskell, static site generators, functional programming, programming"
title: "The hakyll-nix-template Tutorial"
updated: "2026-07-31T12:00:00Z"
---

If you're looking to use [hakyll](https://jaspervdj.be/hakyll) with
[nix](https://nixos.org) to build static sites, this reference article was made
for you!

We will be working with the
[hakyll-nix-template](https://github.com/rpearce/hakyll-nix-template/), so go
ahead and pull that up in a new browser tab. Its README also contains info on
all the features that are provided.

_**Update, July 2026:** I've given the template a solid overhaul since I first
wrote this, so I've gone back through the article to match. The short version:
haskell.nix is gone (it's plain nixpkgs now, so there's no extra cache or trust
setup to fuss with), deploys use GitHub's official Pages action instead of a
`gh-pages` branch, the haskell code is split into small modules instead of one
big `Main.hs`, your editor gets `haskell-language-server` for free, and your
static assets bust their own browser caches. If you've followed along with this
before, it's worth a re-skim._

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

Cachix is optional these days — everything this template needs comes prebuilt
from the [official nix cache](https://cache.nixos.org) — but the GitHub workflow
can push your own builds to your own cache, so I still think it's worth setting
up.

While you're at it, we aren't using [devenv.sh](https://devenv.sh) in this
example, but you should check it out later. As for
[nix-direnv](https://github.com/nix-community/nix-direnv), the template now
ships an `.envrc`, so if you're a direnv person, you're already sorted; more on
that in a bit.

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

Run `nix build`, and then go do something else for a few minutes. The first run
pulls down GHC, hakyll, pandoc, and friends, but they all come prebuilt from the
[official nix cache](https://cache.nixos.org), so how long it takes mostly comes
down to your connection speed; the only thing that gets compiled on your machine
is the site generator itself. There are no substituter trust prompts to answer
anymore, either — that all went away with haskell.nix.

Once that is all done, you'll have a brand new `result/` directory available
that is a symlink to `/nix/store/<HASH>-website/`. For this blog, it looks like
this:

```text
result/
└── dist/
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

```text
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

```text
λ nix develop
[hakyll-nix]λ
```

When you have `[hakyll-nix]λ ` as your prompt, you know that you're in a nix
shell. This comes preloaded with _most_ of your existing CLI tools, plus
`cabal`, `ghc`, `haskell-language-server`, `hlint`, and `ormolu`. If you want it
to be exactly your environment plus the nix develop shell, check out
[nix-direnv](https://github.com/nix-community/nix-direnv); the template ships an
`.envrc`, so all you have to do is run `direnv allow` once.

At this point, if you're using Vim, for example, you can run `vim .` and open
the project up _with access to the aforementioned tools_.

If you're a VS Code person, there's a `.vscode/` folder in the template that
recommends the Haskell and [Nix Env
Selector](https://marketplace.visualstudio.com/items?itemName=arrterian.nix-env-selector)
extensions and points them at `shell.nix`, which is a little bridge to the same
dev shell you just used. The upshot is that `haskell-language-server` runs from
_this_ nix environment instead of downloading a copy of its own. As a bonus, HLS
comes with hlint built in, so you get lint warnings and "apply hint" fixes
without installing anything else. The template's README covers all of this under
"Editor integration (HLS)".

Now, it's time to customize the project for you.

## Personalizing the project build

First, go back to your window where you can `nix run . watch` and cancel that;
e.g., press `ctrl + c`.

All of the haskell code used to live in one big `ssg/src/Main.hs` file, but I've
since broken it up into modules that each do one thing:

```text
ssg/src/
├── Main.hs                          -- wires all the rules together
├── Hakyll/Site/
│   ├── Assets.hs                    -- static asset cache-busting
│   ├── Configuration.hs             -- your site info & hakyll config
│   ├── CustomFields.hs              -- the optional `updated` date field
│   ├── Feed.hs                      -- RSS & Atom feeds
│   ├── Post.hs                      -- post context & title-slug filenames
│   ├── Rules.hs                     -- the build rules & pandoc setup
│   └── Sitemap.hs                   -- sitemap.xml
└── Text/HTML/TagSoup/Compressor.hs  -- squishes the output HTML
```

Everything you need to personalize is in one place, though. Using your editor,
open `ssg/src/Hakyll/Site/Configuration.hs`, and read over the
`PERSONALIZATION` section near the top:

```haskell
data SiteConfiguration = SiteConfiguration
  { siteName :: String
  , siteRoot :: String
  } deriving (Show)

siteConfiguration :: SiteConfiguration
siteConfiguration =
  SiteConfiguration
    { siteName = "My Site Name"
    , siteRoot = "https://my-site.com"
    }

-- https://github.com/jaspervdj/hakyll/blob/66ace430f90ec97cbb9cf278ec46aec3b457fc56/lib/Hakyll/Web/Feed.hs#L69-L81
feedConfiguration :: H.FeedConfiguration
feedConfiguration =
  H.FeedConfiguration
    { H.feedTitle = "My Feed Title"
    , H.feedDescription = "My Site Description"
    , H.feedAuthorName = "My Name"
    , H.feedAuthorEmail = "me@myemail.com"
    , H.feedRoot = "https://my-site.com"
    }
```

These two records contain all the high level, site-based customization text and
root URLs for you to update. Go ahead and do that. Don't fret over whether your
`siteRoot` has a trailing slash on it, by the way; that gets trimmed off for
you, so your URLs won't come out with a double slash in them.

Below this area, you'll find the `CONFIG` section:

```haskell
-- Default configuration: https://github.com/jaspervdj/hakyll/blob/cd74877d41f41c4fba27768f84255e797748a31a/lib/Hakyll/Core/Configuration.hs#L101-L125
hakyllConfiguration :: H.Configuration
hakyllConfiguration =
  H.defaultConfiguration
    { H.destinationDirectory = "dist"
    , H.ignoreFile = ignoreFile'
    , H.previewHost = "127.0.0.1"
    , H.previewPort = 8000
    , H.providerDirectory = "src"
    , H.storeDirectory = "ssg/_cache"
    , H.tmpDirectory = "ssg/_tmp"
    }
  where
    ignoreFile' path
      | ".DS_Store" == fileName           = True
      | "."    `List.isPrefixOf` fileName = False
      | "#"    `List.isPrefixOf` fileName = True
      | "~"    `List.isSuffixOf` fileName = True
      | ".swp" `List.isSuffixOf` fileName = True
      | otherwise = False
      where
        fileName = FP.takeFileName path
```

This section specifically deals with your hakyll config. If you want to change
the development server port, host, content, source directory, what files are or
aren't ignored, and some caching things, then you can do so here.

The rest of the modules are all related to hakyll and the build, so if you know
hakyll already, this should feel familiar, and feel free to customize things
however you like.

Do note that any changes you make inside of `ssg/` means you'll need to turn
your dev server off and on again.

## Adding your first post

Now that we've customized our config, turn the dev server back on with `nix
run . watch`. It's time to add our first post!

Navigate to the `src/posts/` folder and add a new markdown file with this naming
format:

```text
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
        $date$ $if(updated)$(updated: $updated$)$endif$
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

In `ssg/src/Hakyll/Site/Post.hs`, you'll see `postCtx`:

```haskell
postCtx :: H.Context String
postCtx =
  H.constField "root" HSConfig.mySiteRoot
    <> H.constField "feedTitle" HSConfig.myFeedTitle
    <> H.constField "siteName" HSConfig.mySiteName
    <> H.dateField "date" "%Y-%m-%d"
    <> HSCustomFields.updatedField "updated" "%Y-%m-%d"
    <> H.defaultContext
```

This is a post context that gets built up and supplied to the template. Hakyll
has [a special `dateField` helper](https://github.com/jaspervdj/hakyll/blob/909e1b3a89b5b3ba5f64840d23ada9b3ac393404/lib/Hakyll/Web/Template/Context.hs#L295-L343)
that parses a date from your post filename if it begins with a date. It also has
[`defaultContext`](https://github.com/jaspervdj/hakyll/blob/909e1b3a89b5b3ba5f64840d23ada9b3ac393404/lib/Hakyll/Web/Template/Context.hs#L231-L249)
which handles things like your post/web page's body content.

That `updatedField` is one I wrote, and it lives in
`ssg/src/Hakyll/Site/CustomFields.hs`. If a post has an `updated` value in its
front-matter, it parses that (in a few different date formats) and hands your
templates back a tidy `$updated$` date to print; if there isn't one, the
`$if(updated)$` block above simply doesn't render. It's what's producing the
"Revised" row in this very article's info table.

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
outputted build, and this is easily done with hakyll's `copyFileCompiler`. These
days, `ssg/src/Main.hs` is a thin list of rules, and the copying happens in the
`COPY FILES` block:

```haskell
main :: IO ()
main = do
  -- Fingerprint the static assets up front so rendered pages can cache-bust
  -- their URLs with `?v=<hash>` (see Hakyll.Site.Assets).
  manifest <-
    HSAssets.buildManifest
      (H.providerDirectory HSConfig.hakyllConfiguration)
      [ ("css/code.css", HSRules.codeCssContent) ]

  H.hakyllWith HSConfig.hakyllConfiguration $ do
    -- COPY FILES
    H.match "favicon.ico" HSRules.copy
    H.match "robots.txt"  HSRules.copy
    H.match "images/*"    HSRules.copy
    H.match "js/*"        HSRules.copy
    H.match "fonts/*"     HSRules.copy
    -- ...
```

Each file or folder glob here exists inside the `src/` directory. If you have
something you want copied over to the build, this is the place to do it: add a
line like `H.match "pdfs/*" HSRules.copy`, and you're done.

If you find you need to ignore a certain file or extension, consult the
`ignoreFile'` function in `Configuration.hs` and add your problematic file,
prefix, or extension to the guard. For example, my macOS likes to add
`.DS_Store` everywhere, so I did this — and it now ships in the template, so
that's one less thing for you to do:

```haskell
ignoreFile' path
  | ".DS_Store" == fileName           = True -- this line
  | "."    `List.isPrefixOf` fileName = False
  | "#"    `List.isPrefixOf` fileName = True
  | -- ...
```

Two other things you don't have to think about: a `.nojekyll` file gets
generated for you, so GitHub Pages serves your output exactly as hakyll wrote it
(no Jekyll processing, no ignored underscore folders), and every reference to
those static files gets cache-busted for you — which brings us neatly to a TODO
I finally got around to, [further down](#todo-caching-and-hashing).

## Understanding the GitHub action workflow

There GitHub action workflow can be found in `.github/workflows/main.yml`. There
are two jobs here: `build-nix` and `deploy`, and `deploy` only runs on the
`main` branch.

### The `build-nix` job

This is the main job, and it does four things:

1. Install nix
1. Setup the build to run with cachix
1. Run `nix build`
1. Upload the built site (`result/dist`) as a GitHub Pages artifact

### The `deploy` job

When code is pushed to the `main` branch, the `deploy` job will:

1. Run the `build-nix` job
1. Hand that uploaded artifact to GitHub's official
   [`actions/deploy-pages`](https://github.com/actions/deploy-pages) action,
   which publishes it for you

There's no `gh-pages` branch involved anymore, which I'm quite happy about: your
built site goes straight to GitHub Pages, and there's no commit history of
generated files to lug around. The job proves who it is with a short-lived
[OIDC](https://docs.github.com/en/actions/security-for-github-actions/security-hardening-your-deployments/about-security-hardening-with-openid-connect)
token that GitHub mints on the spot, so there's no deploy key or long-lived
token for you to manage.

You'll also notice each action is pinned to a full commit SHA with its version
in a comment next to it, e.g. `actions/checkout@9c091bb... # v7`. Tags can be
moved; commits can't, so this means nobody can swap out the code you're running
from under you. Dependabot updates those pins for you.

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

While you're in the `Settings` tab, go to the `Pages` page and, under `Build and
deployment`, set the `Source` to `GitHub Actions`. That's the whole thing —
there's no branch or directory to pick anymore, since the workflow hands your
built site to GitHub directly.

<img
  alt="The GitHub Pages 'Build and deployment' source dropdown, open, with 'GitHub Actions' selected and 'Deploy from a branch' beneath it"
  decoding="async"
  height="526"
  loading="lazy"
  src="./images/hnt-gh-pages.webp"
  width="684"
/>

Don't skip this one! Until you flip that setting, your `deploy` job will fail,
because your repository isn't expecting deploys from Actions yet.

## Deploying to your domain

On that same `Pages` settings page, there's a `Custom domain` field: pop your
domain in there. The template doesn't ship a `CNAME` file anymore, because
GitHub holds onto that setting for you; if you'd rather keep your domain in
version control, you can still add a `CNAME` file to `src/` and a copy rule for
it in `Main.hs`.

Follow the [GitHub Pages custom domain guide](https://docs.github.com/en/pages/configuring-a-custom-domain-for-your-github-pages-site/managing-a-custom-domain-for-your-github-pages-site)
for heaps of info on how to deploy your site to your web domain.

## TODOs for hakyll-nix-template

### TODO: Caching and hashing

When a CSS or JS file changes, we need a way to break browser caches to ensure
they get the latest version.

**Update, July 2026: I figured it out!** My original plan was to hash a file's
contents, write out a new file with that hash in its name, and then rewrite
every reference to it. What the template does now is a bit simpler: before the
build starts, it hashes everything in your `css`, `js`, `images`, `fonts`, and
`pdfs` folders (plus your `favicon.ico`), and then, as each page gets rendered,
it tacks a `?v=<hash>` query string onto every `href`, `src`, and social image
URL that points at one of those files.

The result is the same — change a file, and only that file's URL changes, so
browsers refetch that one thing and keep the rest — but your output directory
stays free of mystery filenames, and there's nothing for you to wire up in your
templates. Have a look at `ssg/src/Hakyll/Site/Assets.hs` if you're curious
about how it works.

### TODO: Use pygments for syntax highlighting

See [Tony Zorman's post on pygmentising hakyll](https://tony-zorman.com/posts/2023-01-21-pygmentising-hakyll.html)
for details on some issues with the [skylighting
library](https://hackage.haskell.org/package/skylighting).

**Update, July 2026:** I did this for this website, but not for the template.
Following Tony's post, this site pipes its code blocks through
[chroma](https://github.com/alecthomas/chroma) — same idea as pygments, but it's
a single Go binary — and that's what's highlighting the snippets you're reading
right now. The template still uses skylighting, and I'm leaving it that way on
purpose: it needs no external program, so anyone who clicks "Use this template"
gets a site that builds straight away. The template generates its highlighting
stylesheet into `css/code.css` at build time, so it's cache-busted along with
everything else.

## Other hakyll posts

* [Pt. 1 – Setup & Initial Customization](/hakyll-pt-1-setup-initial-customization.html)
* [Pt. 2 – Generating a Sitemap XML File](/hakyll-pt-2-generating-a-sitemap-xml-file.html)
* [Pt. 3 – Generating RSS and Atom XML Feeds](/hakyll-pt-3-generating-rss-and-atom-xml-feeds.html)
* [Pt. 4 – Copying Static Files For Your Build](/hakyll-pt-4-copying-static-files-for-your-build.html)
* [Pt. 5 – Generating Custom Post Filenames From a Title Slug](/hakyll-pt-5-generating-custom-post-filenames-from-a-title-slug.html)
* [Pt. 6 – Pure Builds With Nix](/hakyll-pt-6-pure-builds-with-nix.html)
