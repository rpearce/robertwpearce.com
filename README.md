# robertwpearce.com

My personal website: [https://robertwpearce.com](https://robertwpearce.com).

## Development

1. clone this repository
1. install [nix](https://nixos.org/nix/)
1. list the available commands
   ```sh
   λ ./make help
   ```

### CLI

Print usage:

```sh
λ ./make help
```

Build the project & output:

```sh
λ ./make build
```

Start `nix-shell` for project:

```sh
λ ./make shell
```

Update pinned versions of niv & nixpkgs:

```sh
λ ./make update-pkgs
```

Start a dev server and watch for changes:

```sh
λ ./make shell
[nix-shell:~/projects/robertwpearce.com]$ cd src
[nix-shell:~/projects/robertwpearce.com]$ hakyll-site watch
```

Then go to http://127.0.0.1:8000 in your browser.
