---
author: "Robert Pearce"
authorTwitter: "@RobertWPearce"
desc: "Using Haskell through Nix or Docker might be easier paths on macOS, but this article should help if those options aren't available."
keywords: "haskell, ghcup, macos m1"
title: "One does not simply use GHCup on macOS M1"
updated: "2023-03-22T00:25:00Z"
---

## Intro

The [GHCup](https://www.haskell.org/ghcup/) tool is the official installer for
core [Haskell](https://www.haskell.org) tools: [cabal](https://cabal.readthedocs.io/en/stable/),
[stack](https://docs.haskellstack.org/en/stable/),
[haskell-language-server](https://haskell-language-server.readthedocs.io/en/latest/),
and [ghc](https://www.haskell.org/ghc/).

I usually use Haskell through [Nix](https://nixos.org) (I'm liking
[devenv.sh](https://devenv.sh/), too), and I've also used it through Docker, but
I was frustrated with build times and wanted to try the official Haskell way.

Unfortunately, I had a rough time trying to use GHCup on a macOS M1 (Ventura
13.2.1), so I documented trying to build a small Haskell project of mine,
[slugger](https://hackage.haskell.org/package/slugger), with it.

## A note about Homebrew

I use [Homebrew](https://brew.sh/) for installing all sorts of CLI tools and
apps for macOS (here's my personal
[Brewfile](https://github.com/rpearce/dotfiles/blob/main/conf/Brewfile)).

While I will use it for something else later in this guide, I could not get
`ghcup` to work properly when installed via Homebrew, and trying to upgrade
GHCup through its interface conflicted with the Homebrew install. Instead, I
will use the installer found on [the GHCup page](https://www.haskell.org/ghcup/).

## The library I tried to build

The example library I tried building was my URI slug library,
[slugger](https://github.com/rpearce/slugger).

## Installing GHCup

I like to keep my `$HOME` directory clean by having tools adhere to the [XDG
spec](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html).
I read that if I wanted GHCup to use `XDG`, I needed to export this variable in
the shell where the installer was going to run:

```bash
export GHCUP_USE_XDG_DIRS="true"
```

_[Here are my XDG environment variables.](https://github.com/rpearce/dotfiles/blob/fbda507ce4908ec8689d388c6e4ffe0c4900d318/conf/zsh/.zshenv#L3-L8)_

Since I always want this to be true, [I include that in my `.zshenv` dotfile
just in case](https://github.com/rpearce/dotfiles/blob/fbda507ce4908ec8689d388c6e4ffe0c4900d318/conf/zsh/.zshenv#L22).

Next, I installed GHCup:

```text
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

This is an interactive installer, so there was a bit of output and questions.

<a href="./images/ghcup-install-llvm-note.webp">
  <img
    alt="GHCup installer messages"
    decoding="async"
    height="482"
    loading="lazy"
    src="./images/ghcup-install-llvm-note.webp"
    width="600"
  />
</a>

Tip: if you run this installer, make sure you read the messages.

## Including the GHCup environment

The script asked if it could append something to the end of my `.zshrc`
file. I prefer to own my environment setup, so I let it do its thing, inspected
the file to make sure it looked good, then I changed the sourcing code into a
style I prefer:

```bash
ghcup_script_path="${XDG_DATA_HOME}/ghcup/env"
[[ -f "${ghcup_script_path}" ]] && source "${ghcup_script_path}"
```

This adds some Haskell bin-related directories to `$PATH` if they aren't already
there.

## Running the GHCup terminal user interface

Once this was all done, I opened a new shell window and ran

```text
ghcup tui
```

TUI is an acronym for "terminal user interface".

<a href="./images/ghcup-tui.webp">
  <img
    alt="GHCup terminal user interface"
    decoding="async"
    height="386.5"
    loading="lazy"
    src="./images/ghcup-tui.webp"
    width="600"
  />
</a>

I used the interface to install the recommended tool versions, and this was
really easy! Well done, GHCup crew.

Then I went to go see if I could build `slugger`.

## Building slugger: failure #1 (LLVM)

When I went to the `slugger` project directory, I ran `cabal v2-build`, and some
LLVM errors printed to the screen.

<a href="./images/ghcup-cabal-build-fail-llvm.webp">
  <img
    alt="LLVM not found when trying to build the slugger project"
    decoding="async"
    height="377"
    loading="lazy"
    src="./images/ghcup-cabal-build-fail-llvm.webp"
    width="600"
  />
</a>

Notably:

> Warning: Couldn't figure out LLVM version! Make sure you have installed LLVM
> between [9 and 13]

Remember how I said to make sure you read the installer messages? Yeah. I
didn't.

> On Darwin M1 you might also need a working llvm installed (e.g. via brew) and
> have the toolchain exposed in the PATH.

_Update: User bgamari on lobste.rs had a valuable insight into [why installing
LLVM is recommended by GHCup](https://lobste.rs/s/cbl1yc/one_does_not_simply_use_ghcup_on_macos_m1#c_lcaxqm)._

## Building slugger: failures #2-4 (also LLVM)

As suggested by the warnings above, I added `brew llvm@9` to my `Brewfile`,
installed it, and tried to `cabal v2-build` the `slugger` project.

That didn't work (same sort of issue).

I tried `llvm@10`, `llvm@11`, and `llvm@12`.

None of those worked, either! Would `llvm@13` work? Maybe, maybe, maybe...

## Building slugger: failure #5 (GHC and LLVM)

It seems none of these will work if `ghc` doesn't know to use LLVM.

I keep a [cabal config file in my
dotfiles](https://github.com/rpearce/dotfiles/blob/main/conf/.cabal.conf) and it
had a section, `program-default-options`, that contained a `ghc-options` key for
passing flags to ghc.

Here's how I told GHC about LLVM:

```text
program-default-options
  ghc-options: -fllvm
```

There's more information about that on the [Haskell GHC Backends
doc](https://downloads.haskell.org/ghc/latest/docs/users_guide/codegens.html#llvm-code-generator-fllvm).

Did that make a difference? Yep!

## Building slugger: failure #6 (missing foreign libraries)

Aha! A different error.

<a href="./images/ghcup-cabal-build-fail-icu4c.webp">
  <img
    alt="Missing foreign libraries terminal error"
    decoding="async"
    height="499.5"
    loading="lazy"
    src="./images/ghcup-cabal-build-fail-icu4c.webp"
    width="600"
  />
</a>

> cabal-3.6.2.0 Missing dependencies on foreign libraries:<br />
> Missing (or bad) C libraries: icuuc, icui18n, icudata

This one stemmed from trying to build a dependency,
[text-icu](https://hackage.haskell.org/package/text-icu), and it seemed I was
missing some libraries it expected to find on the OS.

I saw some references on GitHub issues to the `icu4c` tool, but I was luckily
able to find [this archived "Missing dependency on a foreign library"
guide](https://guide.aelve.com/haskell/missing-dependency-on-a-foreign-library-vf6h3d0p#item-a06pn7xw)
that simply told me what to do:

1. `brew install icu4c`
2. If you're using stack, add this to `~/.stack/config.yaml`:
   ```text
   extra-include-dirs:
   - /usr/local/opt/icu4c/include

   extra-lib-dirs:
   - /usr/local/opt/icu4c/lib
   ```

Unfortunately, none of this worked out of the box for me for two reasons:

1. I'm not using `stack`
2. Homebrew uses `/opt/homebrew/` for Apple Silicon—not `/usr/local/`

But those config options looked _exactly the same_ as the recommendation from
the build warning above, and that gave me some things to try:

> If the libraries are already installed but in a non-standard location then you
> can use the flags `--extra-include-dirs=` and `--extra-lib-dirs=` to specify where
> they are.

## Fixing the missing foreign libraries issue

It turns out that my `cabal.conf` file  had `extra-include-dirs` and
`extra-lib-dirs` in it, so I didn't need to pass paths every time I tried to
build with cabal.

I don't regularly edit cabal config files, so I took the `stack` _YAML_ config
above and tried it:

```text
extra-include-dirs:
- /opt/homebrew/opt/icu4c/include

extra-lib-dirs:
- /opt/homebrew/opt/icu4c/lib
```

Nope, that didn't work. I tried indenting the `- ` to see if the config file
liked that.

Nope.

While this config file might, at a glance, resemble YAML, it isn't—it seems to
resemble (or even be) a `.cabal` file (<a href="mailto:me@robertwpearce.com">email me</a> if
you know, please!). Here was a correct way to write them:

```text
extra-include-dirs:
  /opt/homebrew/opt/icu4c/include

extra-lib-dirs:
  /opt/homebrew/opt/icu4c/lib
```

## Sweet success

With high hopes, I ran `cabal v2-build` again, and it worked!

<a href="./images/ghcup-cabal-build-success.webp">
  <img
    alt="Successful build result and test of the slugger library"
    decoding="async"
    height="482"
    loading="lazy"
    src="./images/ghcup-cabal-build-success.webp"
    width="600"
  />
</a>

I was successfully able to build my little library and test it out with `cabal`.

## Personal retrospective on the experience

There are a number of places here where, if I'd have paid closer attention to
(admittedly helpful) walls of text, I'd have been led to solutions faster.
That is unquestionably my fault!

That said, the errors don't cover everything you have to do (like the `-fllvm`
GHC flag), and this overall experience on macOS was rough for me.

**I am grateful for all the effort put into GHCup, and I know it takes time and
money to make things simple.**

For now, even though Nix's story isn't one of simplicity, either, I'm going to
mostly stick with building Haskell projects that way. However, I'll keep my
options open and periodically try things the GHCup way, as well.

* * *

Thanks for reading!<br />
— Robert
