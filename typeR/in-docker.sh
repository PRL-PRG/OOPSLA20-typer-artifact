#!/usr/bin/env bash
set -euo pipefail

script_dir="$( cd "$(dirname "${BASH_SOURCE[0]}")" ; pwd -P )"
base_dir=$(dirname "$script_dir")

docker run \
    -ti \
    --rm \
    -e ROOT=TRUE \
    -e DISABLE_AUTH=true \
    -e USERID=$(id -u) \
    -e GROUPID=$(id -g) \
    -v "$base_dir/typeR:/home/rstudio/typeR" \
    -v "$base_dir/README.Rmd:/home/rstudio/README.Rmd" \
    -v "$base_dir/.Rprofile:/home/rstudio/.Rprofile" \
    -w /home/rstudio/typeR \
    prlprg/oopsla20-typer \
    "$@"
