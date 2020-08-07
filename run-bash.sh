#!/bin/bash

docker run \
  -ti \
  --rm \
  -v "$$(pwd)/typeR:/typeR" \
  prlprg/oopsla20-typer \
  bash
