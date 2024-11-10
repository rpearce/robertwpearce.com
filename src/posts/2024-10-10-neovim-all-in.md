---
author: "Robert Pearce"
authorTwitter: "@RobertWPearce"
desc: "I went all-in on neovim tooling, and it went very well (includes screenshots)."
keywords: "neovim, lua, lazy.nvim, config, nvim"
title: "Neovim: All In"
updated: "2024-11-11T12:00:00Z"
---

This is an accounting of how I recently went all-in on
[Neovim](https://neovim.io) with its Lua-based configuration ecosystem.

Here is my [nvim config at time of
writing](https://github.com/rpearce/dotfiles/tree/a2c9e46bb413039cbf98d7ea1ab90bf6ca023178/tools/conf/nvim).
This folder just needs to live in `$XDG_CONFIG_HOME` (typically, `~/.config`),
and in my case, [I symlink it from my dotfiles to there](https://github.com/rpearce/dotfiles/blob/a2c9e46bb413039cbf98d7ea1ab90bf6ca023178/tools/conf/install#L55).

In this short post, I'll share about the switch, some screenshots, and list the
tools I'm using at the time of writing.

## Backstory

I've used Neovim for years now, but I've always had one
foot in the [VIM](https://www.vim.org) world: a somewhat lengthy `.vimrc`
(standard fare), plus [vim-plug](https://github.com/junegunn/vim-plug) for
managing plugins. I also used [ale](https://github.com/dense-analysis/ale) and
[coc-nvim](https://github.com/neoclide/coc.nvim) for managing linters and LSPs.

This worked great for years, but I ran into an issue at work where `ale` and
`coc-nvim` seemed to no longer play well together. After trying to fix it in my
spare time and failing, I decided to completely burn things down and start fresh
with _The Neovim Way_.

## How the switch went

Most of the experience was dancing between reading official docs, reading blog
posts, watching YouTube videos, reading source code, searching through GitHub
issues, noting `CHANGELOG` differences since these kindly-written resources
were created, etc. It was a complete hodgepodge approach, but it's typically how
I learn things, and it worked out over ~5 nights.

If you want a breakdown of how to structure your files and write plugin configs,
send me an email, and I'll consider writing some more about this!

## The verdict

This tooling is incredible: the configs are almost completely declarative,
updating tools is easy as pie, and it is all unbelievably fast.

At work, the ability to lint and typecheck a massive JS/TS project immediately,
while I'm typing (and without any weird UI freezes...looking at you, VSCode), is
critical. I am especially surprised by how good
[Telescope](https://github.com/nvim-telescope/telescope.nvim) is; it's file
finding and live grep are _fast_ and quite pleasant to use.

## Screenshots

<a href="./images/neovim-01-home.webp">
  <img
    alt="neovim start screen with customized alpha.nvim"
    decoding="async"
    height="1740"
    loading="lazy"
    src="./images/neovim-01-home.webp"
    width="3022"
  />
</a>

<a href="./images/neovim-02-code-tree.webp">
  <img
    alt="neotree sidebar plus haskell code example"
    decoding="async"
    height="1818"
    loading="lazy"
    src="./images/neovim-02-code-tree.webp"
    width="3024"
  />
</a>

<a href="./images/neovim-03-ff.webp">
  <img
    alt="fuzzy file finding with telescope"
    decoding="async"
    height="1820"
    loading="lazy"
    src="./images/neovim-03-ff.webp"
    width="3024"
  />
</a>

<a href="./images/neovim-04-fg.webp">
  <img
    alt="fuzzy live grep with telescope"
    decoding="async"
    height="1818"
    loading="lazy"
    src="./images/neovim-04-fg.webp"
    width="3024"
  />
</a>

<a href="./images/neovim-05-lazy.webp">
  <img
    alt="lazy.nvim interface"
    decoding="async"
    height="1816"
    loading="lazy"
    src="./images/neovim-05-lazy.webp"
    width="3024"
  />
</a>

<a href="./images/neovim-06-mason.webp">
  <img
    alt="mason interface"
    decoding="async"
    height="1818"
    loading="lazy"
    src="./images/neovim-06-mason.webp"
    width="3024"
  />
</a>

## List of the tools I used

Tool managers:

* [Lazy.nvim](https://github.com/folke/lazy.nvim) (this underpins just about
  everything else)
* [williamboman/mason.nvim](https://github.com/williamboman/mason.nvim)
* [WhoIsSethDaniel/mason-tool-installer.nvim](https://github.com/WhoIsSethDaniel/mason-tool-installer.nvim)

Look and feel:

* [goolord/alpha-nvim](https://github.com/goolord/alpha-nvim) (neovim start page)
* [nvim-lualine/lualine.nvim](https://github.com/nvim-lualine/lualine.nvim)
  (statusline)
* [lewis6991/gitsigns.nvim](https://github.com/lewis6991/gitsigns.nvim) (buffer
  git integration)
* [dracula/vim](https://github.com/dracula/vim) (theme)
* [folke/tokyonight.nvim](https://github.com/folke/tokyonight.nvim) (theme)

Sidebar folder tree:

* [vim-neo-tree/neo-tree.nvim](https://github.com/nvim-neo-tree/neo-tree.nvim)

Language things:

* [neovim/nvim-lspconfig](https://github.com/neovim/nvim-lspconfig)
* [williamboman/mason-lspconfig.nvim](https://github.com/williamboman/mason-lspconfig.nvim)
* [nvim-treesitter/nvim-treesitter](https://github.com/nvim-treesitter/nvim-treesitter)
* [norcalli/nvim-colorizer.lua](https://github.com/norcalli/nvim-colorizer.lua)
* [hrsh7th/cmp-nvim-lsp](https://github.com/hrsh7th/cmp-nvim-lsp)
* [hrsh7th/nvim-cmp](https://github.com/hrsh7th/nvim-cmp)
* [mfussenegger/nvim-lint](https://github.com/mfussenegger/nvim-lint)
* [stevearc/conform.nvim](https://github.com/stevearc/conform.nvim)

Finding things:

* [nvim-telescope/telescope.nvim](https://github.com/nvim-telescope/telescope.nvim)

Misc:

* [folke/todo-comments.nvim](https://github.com/folke/todo-comments.nvim)
* [kylechui/nvim-surround](https://github.com/kylechui/nvim-surround)

## 

* * *

Thanks for reading!<br />
— Robert
