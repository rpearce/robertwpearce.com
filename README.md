# robertwpearce.com
My personal website: [https://robertwpearce.com](https://robertwpearce.com).

## Development
1. clone this repository
1. install [nix](https://nixos.org/nix/)
1. list the available commands
   ```
   λ ./make.sh help
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
