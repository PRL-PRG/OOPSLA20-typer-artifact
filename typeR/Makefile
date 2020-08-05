ifndef IN_DOCKER
$(error Targets in this makefile are supposed to be run in a docker container. Try prefixing with ./run.sh)
endif

ENVIRONMENT_FILE = /envir
JOBS ?= 90%
JOBS_FILE := jobs-file.txt
PACKAGES_FILE ?= $(R_PROJECT_BASE_DIR)/packages-corpus.txt
TIMEOUT ?= 30m

MERGE_CSV := $(R_PROJECT_BASE_DIR)/runr/inst/merge-csv.R

this_makefile := $(lastword $(MAKEFILE_LIST))

.PHONY: \
  extract-code-propagatr \
  on-each-package \
  package-metadata

on-each-package := $(MAKE) -f $(this_makefile) on-each-package

task/package-runnable-code%: export TARGETS = runnable-code.csv runnable-code-metadata.csv

task/package-metadata: export TARGETS = metadata.csv

task/run-extracted-code: task/package-runnable-code-propagatr
task/run-extracted-code: export TARGETS = run.csv
task/run-extracted-code: export OUTPUT_DIR = $(RUN_DIR)/run-extracted-code-propagatr
task/run-extracted-code: export ENVIRONMENT_FILE = /envir-dyntrace
task/run-extracted-code: export ARGS = $(RUN_DIR)/package-runnable-code-propagatr/{1/}

task/%: export OUTPUT_DIR = $(RUN_DIR)/$(@F)
task/%:
	$(on-each-package) TASK=$(@F).R ENVIRONMENT_FILE=$(ENVIRONMENT_FILE)
	$(MERGE_CSV) "$(OUTPUT_DIR)" $(TARGETS)

on-each-package:
	@[ "$(TASK)" ] || ( echo "*** Undefined TASK"; exit 1 )
	@[ "$(OUTPUT_DIR)" ] || ( echo "*** Undefined OUTPUT_DIR"; exit 1 )
	@echo "$(JOBS)" > $(JOBS_FILE)
	-mkdir -p "$(OUTPUT_DIR)"
	-. $(ENVIRONMENT_FILE); \
  parallel \
    -a $(PACKAGES_FILE) \
    --bar \
    --jobs "$(JOBS_FILE)" \
    --results "$(OUTPUT_DIR)/parallel.csv" \
    --tagstring "$(TASK) - {/}" \
    --timeout $(TIMEOUT) \
    --workdir "$(OUTPUT_DIR)/{/}/" \
    $(R_PROJECT_BASE_DIR)/runr/inst/run-task.sh \
      "$(R_PROJECT_BASE_DIR)/runr/inst/tasks/$(TASK)" \
      "$(PACKAGES_SRC_DIR)/{1/}" $(ARGS)
