.PHONY: image image-upload

# TODO make a macro
RUNR_VERSION := $(shell 2>/dev/null curl -s https://api.github.com/repos/PRL-PRG/runr/commits?sha=typer-oopsla20 | 2>/dev/null jq -r '.[0].sha')
ifndef RUNR_VERSION
RUNR_VERSION := $(shell date +%s)
endif

CONTRACTR_VERSION := $(shell 2>/dev/null curl -s https://api.github.com/repos/PRL-PRG/contractr/commits?sha=typer-oopsla20 | 2>/dev/null jq -r '.[0].sha')
ifndef CONTRACTR_VERSION
CONTRACTR_VERSION := $(shell date +%s)
endif

image:
	docker build \
    --rm \
    --build-arg RUNR_VERSION=$(RUNR_VERSION) \
    --build-arg CONTRACTR_VERSION=$(CONTRACTR_VERSION) \
    -t prlprg/oopsla20-typer \
    .

image-upload: image
	docker push prlprg/oopsla20-typer

