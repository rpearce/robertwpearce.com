#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o pipefail

function build() {
  build-cabal && build-project && build-output
  return 0
}

function build-cabal() {
  nix-shell --pure -p cabal2nix --run "cabal2nix ." > project.nix
  return 0
}

function build-output() {
  yarn --cwd ./js build
  site rebuild
  return 0
}

function build-project() {
  nix-build
  return 0
}

function nixpkgs-update() {
  nix-shell -p niv --run "niv update nixpkgs"
  return 0
}

function no-cmd() {
  echo "Missing: COMMAND"
  echo ""
  usage
  return 1
}

function repl() {
  nix-shell --pure --run "cabal new-repl"
  return 0
}

function serve() {
  if ! type "npx" &> /dev/null; then
    echo "Error: npx not found. Please install nodejs to use this command."
    return 1
  fi

  npx serve $@ ./docs
  return 0
}

function site() {
  result/bin/site $@
  return 0
}

function unknown-cmd() {
  echo "Unknown command: $@"
  echo ""
  usage
  return 1
}

function usage() {
  cat <<EOF
Usage: $0 COMMAND

Available commands:
  build             Build the project & output
  build-cabal       Update project.nix from .cabal contents
  build-output      Build the output
  build-project     Use nix to build the project
  help              Print usage
  nixpkgs-update    Update pinned version of nixpkgs
  repl              Start interactive REPL for project
  site              Run hakyll commands
  serve             Serve up the output at a port
EOF
  return 0
}

function err-site() {
  echo "No executable at ./result/bin/site"
  echo ""
  echo "Try running ./make.sh build"
  return 1
}

# Check if no command is provided

[[ $# -lt 1 ]] && no-cmd

# Determine command

cmd="$1"
shift

case "$cmd" in
  build)
    build
    ;;
  build-output)
    build-output
    ;;
  build-cabal)
    build-cabal
    ;;
  build-project)
    build-project
    ;;
  help)
    usage
    ;;
  nixpkgs-update)
    nixpkgs-update
    ;;
  repl)
    repl
    ;;
  serve)
    serve $@
    ;;
  site)
    [[ -f ./result/bin/site ]] || err-site
    site $1
    ;;
  *)
    unknown-cmd cmd
    ;;
esac
