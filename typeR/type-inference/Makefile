ifndef IN_DOCKER
$(error Targets in this makefile are supposed to be run in a docker container. Try prefixing with ./run.sh)
endif

# extra parameters
JOBS          ?= 1
PACKAGES_FILE ?= packages-small-corpus.txt
TIMEOUT       ?= 30m
R_PROJECT_BASE_DIR ?= /R

# environment
R_DIR              := $(R_PROJECT_BASE_DIR)/R-3.5.0
RDT_DIR            := $(R_PROJECT_BASE_DIR)/R-dyntrace
RUNR_DIR           := $(R_PROJECT_BASE_DIR)/runr
RUNR_TASKS_DIR     := $(RUNR_DIR)/inst/tasks
PACKAGES_SRC_DIR   := $(R_PROJECT_BASE_DIR)/CRAN/extracted
PACKAGES_ZIP_DIR   := $(R_PROJECT_BASE_DIR)/CRAN/src/contrib
CRAN_LOCAL_MIRROR  := file://$(R_PROJECT_BASE_DIR)/CRAN
RUN_DIR            := $(CURDIR)/run

# variables
MERGE_CSV := $(RUNR_DIR)/inst/merge-csv.R
ON_EACH_PACKAGE := $(MAKE) on-each-package

.PHONY: \
  on-each-package \
  package-asserts \
  package-functions \
  propagatr-code \
  propagatr-run

clean:
	-rm -fr $(RUN_DIR)

CONSOLIDATE_TYPES_CSV := $(RUN_DIR)/consolidate-types/types.csv
PACKAGE_ASSERTS_CSV   := $(RUN_DIR)/package-asserts/asserts.csv
PACKAGE_FUNCTIONS_CSV := $(RUN_DIR)/package-functions/functions.csv
PROPAGATR_CODE_CSV    := $(RUN_DIR)/propagatr-code/runnable-code.csv
PROPAGATR_RUN_CSV     := $(RUN_DIR)/propagatr-run/run.csv

$(PACKAGE_FUNCTIONS_CSV): export OUTPUT_DIR=$(@D)
$(PACKAGE_FUNCTIONS_CSV):
	$(ON_EACH_PACKAGE) TASK=$(RUNR_TASKS_DIR)/package-functions.R
	$(MERGE_CSV) "$(OUTPUT_DIR)" $(@F)

$(PACKAGE_ASSERTS_CSV): export OUTPUT_DIR=$(@D)
$(PACKAGE_ASSERTS_CSV):
	$(ON_EACH_PACKAGE) TASK=$(RUNR_TASKS_DIR)/package-asserts.R
	$(MERGE_CSV) "$(OUTPUT_DIR)" $(@F)

$(PROPAGATR_CODE_CSV): export OUTPUT_DIR=$(@D)
$(PROPAGATR_CODE_CSV):
	$(ON_EACH_PACKAGE) TASK=$(RUNR_TASKS_DIR)/package-runnable-code-propagatr.R
	$(MERGE_CSV) "$(OUTPUT_DIR)" $(@F) runnable-code-metadata.csv

$(PROPAGATR_RUN_CSV): $(PROPAGATR_CODE_CSV)
$(PROPAGATR_RUN_CSV): export OUTPUT_DIR=$(@D)
$(PROPAGATR_RUN_CSV): export START_XVFB=1
$(PROPAGATR_RUN_CSV):
	$(ON_EACH_PACKAGE) R_DIR=$(RDT_DIR) TASK=$(RUNR_TASKS_DIR)/run-extracted-code.R ARGS="$(dir $(PROPAGATR_CODE_CSV))/{1/}"
	$(MERGE_CSV) "$(OUTPUT_DIR)" $(@F)

$(CONSOLIDATE_TYPES_CSV): $(PACKAGE_FUNCTIONS_CSV) $(PROPAGATR_RUN_CSV)
$(CONSOLIDATE_TYPES_CSV): export OUTPUT_DIR=$(@D)
$(CONSOLIDATE_TYPES_CSV):
	$(ON_EACH_PACKAGE) TASK=$(CURDIR)/scripts/consolidate-types.R \
    ARGS="$(dir $(PROPAGATR_RUN_CSV)) $(dir $(PACKAGE_FUNCTIONS_CSV))"
	$(MERGE_CSV) "$(OUTPUT_DIR)" $(@F)

consolidate-types: $(CONSOLIDATE_TYPES_CSV)
package-asserts:   $(PACKAGE_ASSERTS_CSV)
package-functions: $(PACKAGE_FUNCTIONS_CSV)
propagatr-code:    $(PROPAGATR_CODE_CSV)
propagatr-run:     $(PROPAGATR_RUN_CSV)

on-each-package:
	@[ "$(TASK)" ] || ( echo "*** Undefined TASK"; exit 1 )
	@[ -x "$(TASK)" ] || ( echo "*** $(TASK): no such file"; exit 1 )
	@[ "$(OUTPUT_DIR)" ] || ( echo "*** Undefined OUTPUT_DIR"; exit 1 )
	@[ -z "$(START_XVFB)" ] || $(RUNR_DIR)/inst/start-xvfb.sh

	-rm -fr "$(OUTPUT_DIR)"
	-mkdir -p "$(OUTPUT_DIR)"
	-PATH=$$R_DIR/bin:$$PATH; \
  parallel \
    -a $(PACKAGES_FILE) \
    --bar \
    --env PATH \
    --jobs $(JOBS) \
    --results "$(OUTPUT_DIR)/parallel.csv" \
    --tagstring "$(notdir $(TASK)) - {/}" \
    --timeout $(TIMEOUT) \
    --workdir "$(OUTPUT_DIR)/{/}/" \
    $(RUNR_DIR)/inst/run-task.sh \
      $(TASK) "$(PACKAGES_SRC_DIR)/{1/}" $(ARGS)
