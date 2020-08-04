.PHONY: docker

run:
	docker run \
    -ti \
    --rm \
    --name oopsla20-typer-artifact \
    -e ROOT=TRUE \
    -e DISABLE_AUTH=true \
    -e USERID=$$(id -u) \
    -e GROUPID=$$(getent group r | cut -d: -f3) \
    -p "8787:8787" \
    -v "$$(pwd)/typeR:/home/rstudio/typeR" \
    -v "$$(pwd)/README.Rmd:/home/rstudio/README.Rmd" \
    -v "$$(pwd)/.Rprofile:/home/rstudio/.Rprofile" \
    prlprg/oopsla20-typer \
    /init

.docker-build:
	docker build \
    --rm \
    -t prlprg/oopsla20-typer \
    .

.docker-run-bash:
	docker run \
    -ti \
    --rm \
    -v "$$(pwd)/typeR:/typeR" \
    prlprg/oopsla20-typer \
    bash

.docker-upload: docker
	docker push prlprg/oopsla20-typer
