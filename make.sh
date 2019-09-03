#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o pipefail

function build() {
  build-cabal && build-project && site build
  return 0
}

function build-cabal() {
  nix-shell --pure -p cabal2nix --run "cabal2nix ." > project.nix
  return 0
}

function build-project() {
  nix-build
  return 0
}

function nixpkgs-update() {
  nix-shell -p nix-prefetch-git --run "nix-prefetch-git https://github.com/NixOS/nixpkgs > nixpkgs.json" > project.nix
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
  build-project     Use nix to build the project
  help              Print usage
  nixpkgs-update    Update pinned version of nixpkgs
  repl              Start interactive REPL for project
  site              Run hakyll commands
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
  "build")
    build
    ;;
  "build-cabal")
    build-cabal
    ;;
  "build-project")
    build-project
    ;;
  "help")
    usage
    ;;
  "repl")
    repl
    ;;
  "site")
    [[ -f ./result/bin/site ]] || err-site
    site $@
    ;;
  "nixpkgs-update")
    nixpkgs-update
    ;;
  *)
    unknown-cmd cmd
    ;;
esac
