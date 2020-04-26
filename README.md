# robertwpearce.com
My personal website: [https://robertwpearce.com](https://robertwpearce.com).

## Development
1. clone this repository
1. install [nix](https://nixos.org/nix/)
1. list the available commands
   ```
   λ ./make help
   ```

### CLI
Print usage:
```
λ ./make help
```

Build the project & output:
```
λ ./make build
```

Run hakyll commands (see next section):
```
λ ./make site <COMMAND>
```

Use nix to build the project:
```
λ ./make build-project
```

Start interactive REPL for project:
```
λ ./make repl
```

Update pinned version of nixpkgs:
```
λ ./make nixpkgs-update
```

### Helpful hakyll commands
Build the site:
```
λ ./make site build
```

Rebuild the site:
```
λ ./make site rebuild
```

Start a dev server and watch for changes:
```
λ ./make site watch
λ open http://localhost:8000
```

Clean up site and remove cache:
```
λ ./make site clean
```
