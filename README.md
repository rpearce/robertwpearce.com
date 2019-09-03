# robertwpearce.com
My personal website: [https://robertwpearce.com](https://robertwpearce.com).

## Development
1. clone this repository
1. install [nix](https://nixos.org/nix/)
1. list the available commands
   ```
   λ ./make.sh help
   ```

### CLI
Print usage:
```
λ ./make.sh help
```

Build the project & output:
```
λ ./make.sh build
```

Run hakyll commands (see next section):
```
λ ./make.sh site <COMMAND>
```

Update project.nix from .cabal contents:
```
λ ./make.sh build-cabal
```

Use nix to build the project:
```
λ ./make.sh build-project
```

Start interactive REPL for project:
```
λ ./make.sh repl
```

Update pinned version of nixpkgs:
```
λ ./make.sh nixpkgs-update
```

### Helpful hakyll commands
Build the site:
```
λ ./make.sh site build
```

Rebuild the site:
```
λ ./make.sh site rebuild
```

Start a dev server and watch for changes:
```
λ ./make.sh site watch
λ open http://localhost:8000
```

Clean up site and remove cache:
```
λ ./make.sh site clean
```
