#!/bin/bash

set -o errexit
set -o nounset
set -eou pipefail

function build() {
  npm run build --prefix js
  nix-build --show-trace
  return 0
}

function no-cmd() {
  echo "Missing: COMMAND"
  echo ""
  usage
  return 1
}

function shell() {
  nix-shell --pure
  return 0
}

function unknown-cmd() {
  echo "Unknown command: $*"
  echo ""
  usage
  return 1
}

function update-pkgs() {
  nix-shell --pure --run "niv update"
  return 0
}

function usage() {
  cat <<EOF
Usage: $0 COMMAND

Available commands:
  build             Build the project & output
  help              Print usage
  shell             Run nix-shell --pure
  update-pkgs       Update pinned versions of niv & nixpkgs
EOF
  return 0
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
  help)
    usage
    ;;
  shell)
    shell
    ;;
  update-pkgs)
    update-pkgs
    ;;
  *)
    unknown-cmd cmd
    ;;
esac
