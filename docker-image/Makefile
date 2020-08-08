.PHONY: image image-upload

RUNR_VER := $(shell 2>/dev/null curl -s https://api.github.com/repos/PRL-PRG/runr/commits?sha=typer-oopsla20 | 2>/dev/null jq -r '.[0].sha')
ifndef RUNR_VER
RUNR_VER := $(shell date +%s)
endif

image:
	docker build \
    --rm \
    --build-arg RUNR_VER=$(RUNR_VER) \
    -t prlprg/oopsla20-typer \
    .

image-upload: image
	docker push prlprg/oopsla20-typer

