#!/bin/bash

DEF_PORT=8787
DEF_CMD=/init

function show_help() {
    echo "Usage: $(basename $0) [-p NUM ] [CMD]"
    echo
    echo "where:"
    echo
    echo "  -p NUM      (optional) port for RStudio (defaults to $DEF_PORT)"
    echo "  CMD         (optional) command to run (defaults to RStudio)"
    echo
}

port=$DEF_PORT
cmd=$DEF_CMD

while getopts "hp:" opt; do
    case "$opt" in
    h)
        show_help
        exit 0
        ;;
    p)  port=$OPTARG
        ;;
    esac
done

shift $((OPTIND -1))

[ $# -gt 0 ] && cmd="$@"

docker run \
  -ti \
  --rm \
  --name oopsla20-typer-artifact \
  -e ROOT=TRUE \
  -e DISABLE_AUTH=true \
  -e USERID=$(id -u) \
  -e GROUPID=$(getent group r | cut -d: -f3) \
  -p "$port:8787" \
  -v "$(pwd)/typeR:/home/rstudio/typeR" \
  -v "$(pwd)/README.Rmd:/home/rstudio/README.Rmd" \
  -v "$(pwd)/.Rprofile:/home/rstudio/.Rprofile" \
  prlprg/oopsla20-typer \
  "$cmd"
