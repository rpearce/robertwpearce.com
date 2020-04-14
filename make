#!/usr/bin/env bash

set -o errexit
set -o nounset
set -eou pipefail

function build() {
  build-cabal
  build-project
  build-output
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
  nix-build --show-trace
  return 0
}

function no-cmd() {
  echo "Missing: COMMAND"
  echo ""
  usage
  return 1
}

function repl() {
  nix-shell --pure --run "ghci"
  return 0
}

function shell() {
  nix-shell --pure
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

function update-niv() {
  nix-shell --pure --run "niv update niv"
  return 0
}

function update-nixpkgs() {
  nix-shell --pure --run "niv update nixpkgs"
  return 0
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
  repl              Start interactive Haskell REPL for project
  shell             Run nix-shell --pure
  site              Run hakyll commands
  update-niv        Update pinned version of niv
  update-nixpkgs    Update pinned version of nixpkgs
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
  repl)
    repl
    ;;
  shell)
    shell
    ;;
  site)
    [[ -f ./result/bin/site ]] || err-site
    site $1
    ;;
  update-niv)
    update-niv
    ;;
  update-nixpkgs)
    update-nixpkgs
    ;;
  *)
    unknown-cmd cmd
    ;;
esac
