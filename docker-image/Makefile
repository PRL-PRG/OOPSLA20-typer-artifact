.PHONY: image image-upload

image:
	docker build \
    --rm \
    --build-arg RUNR_VER="$$(date +%s)" \
    -t prlprg/oopsla20-typer \
    .
image-upload: docker
	docker push prlprg/oopsla20-typer

