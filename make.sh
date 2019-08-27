#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o pipefail

function build-cabal() {
  nix-shell --pure -p cabal2nix --run "cabal2nix ." > project.nix
  return 0
}

function build-project() {
  nix-build
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

function run-tests() {
  echo "Tests not implemented yet"
  #nix-shell --pure --run "cabal new-test all"
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
  build-all        Build the project & output
  build-cabal      Update project.nix from .cabal contents
  build-project    Use nix to build the project
  help             Print usage
  repl             Start interactive REPL for project
  site             Run hakyll commands
  test             Run tests
EOF
  return 0
}

# Check if no command is provided

[[ $# -lt 1 ]] && no-cmd

# Determine command

cmd="$1"
shift

case "$cmd" in
  "build-all")
    build-cabal && build-project && site build
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
    site $@
    ;;
  "test")
    run-tests
    ;;
  *)
    unknown-cmd cmd
    ;;
esac
