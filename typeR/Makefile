ifndef IN_DOCKER
$(error Targets in this makefile are supposed to be run in a docker container. Try prefixing with ./run.sh)
endif

# extra parameters
JOBS          ?= 90%
PACKAGES_FILE ?= /R/packages-corpus.txt
TIMEOUT       ?= 30m

# environment
R_DIR              := /R/R-3.5.0
RDT_DIR            := /R/R-dyntrace
RUNR_DIR           := /R/runr
PACKAGES_SRC_DIR   := /R/CRAN/extracted
PACKAGES_ZIP_DIR   := /R/CRAN/src/contrib
CRAN_LOCAL_MIRROR  := file:///R/CRAN
RUN_DIR            := /typeR/run

# variables
JOBS_FILE := jobs-file.txt
MERGE_CSV := $(RUNR_DIR)/inst/merge-csv.R

.PHONY: \
  on-each-package

clean:
	-rm -fr $(RUN_DIR)

task/package-metadata: export TARGETS = metadata.csv

task/package-runnable-code: export TARGETS = runnable-code.csv runnable-code-metadata.csv

task/package-runnable-code-propagatr: export TARGETS = runnable-code.csv runnable-code-metadata.csv

task/run-extracted-code: run/package-runnable-code/runnable-code.csv
task/run-extracted-code: export ARGS = $(RUN_DIR)/package-runnable-code/{1/}
task/run-extracted-code: export TARGETS = run.csv
task/run-extracted-code: export TASK = run-extracted-code.R
task/run-extracted-code: export START_XVFB = 1

task/run-extracted-code-propagatr: run/package-runnable-code-propagatr/runnable-code.csv
task/run-extracted-code-propagatr: export ARGS = $(RUN_DIR)/package-runnable-code-propagatr/{1/}
task/run-extracted-code-propagatr: export OUTPUT_DIR = $(RUN_DIR)/run-extracted-code-propagatr
task/run-extracted-code-propagatr: export R_DIR = $(RDT_DIR)
task/run-extracted-code-propagatr: export TARGETS = run.csv
task/run-extracted-code-propagatr: export TASK = run-extracted-code.R
task/run-extracted-code-propagatr: export START_XVFB = 1

task/%: export TASK = $(@F).R
task/%: export OUTPUT_DIR = $(RUN_DIR)/$(@F)
task/%:
	$(MAKE) on-each-package R_DIR=$(R_DIR)
	$(MERGE_CSV) "$(OUTPUT_DIR)" $(TARGETS)

on-each-package: TASK_BIN = $(RUNR_DIR)/inst/tasks/$(notdir $(TASK))
on-each-package:
	@[ "$(TASK)" ] || ( echo "*** Undefined TASK"; exit 1 )
	@[ -f "$(TASK_BIN)" ] || ( echo "*** Unknown TASK_BIN ($(TASK_BIN))"; exit 1 )
	@[ "$(OUTPUT_DIR)" ] || ( echo "*** Undefined OUTPUT_DIR"; exit 1 )
	@[ -z "$(START_XVFB)" ] || $(RUNR)/inst/start-xvfb.sh
	@echo "$(JOBS)" > $(JOBS_FILE)
	-mkdir -p "$(OUTPUT_DIR)"
	PATH=$$R_DIR/bin:$$PATH; \
  echo $$PATH; \
  parallel \
    -a $(PACKAGES_FILE) \
    --bar \
    --env PATH \
    --jobs "$(JOBS_FILE)" \
    --results "$(OUTPUT_DIR)/parallel.csv" \
    --tagstring "$(TASK) - {/}" \
    --timeout $(TIMEOUT) \
    --workdir "$(OUTPUT_DIR)/{/}/" \
    $(TASK_BIN) \
      "$(PACKAGES_SRC_DIR)/{1/}" $(ARGS)
