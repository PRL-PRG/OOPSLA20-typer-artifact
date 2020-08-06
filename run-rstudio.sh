#!/bin/bash

PORT=8787

[ $# -eq 1 ] && PORT=$1

docker run \
  -ti \
  --rm \
  --name oopsla20-typer-artifact \
  -e ROOT=TRUE \
  -e DISABLE_AUTH=true \
  -e USERID=$(id -u) \
  -e GROUPID=$(getent group r | cut -d: -f3) \
  -p "$PORT:8787" \
  -v "$(pwd)/typeR:/home/rstudio/typeR" \
  -v "$(pwd)/README.Rmd:/home/rstudio/README.Rmd" \
  -v "$(pwd)/.Rprofile:/home/rstudio/.Rprofile" \
  prlprg/oopsla20-typer \
  /init
