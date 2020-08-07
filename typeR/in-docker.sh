#!/usr/bin/env bash
set -euo pipefail

base_dir="$( cd "$(dirname "${BASH_SOURCE[0]}")" ; pwd -P )"

docker run \
    -ti \
    --rm \
    -v "$base_dir:/typeR" \
    -w /typeR \
    prlprg/oopsla20-typer \
    "$@"
