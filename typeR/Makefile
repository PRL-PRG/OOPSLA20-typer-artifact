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
SCRIPTS_DIR        := $(CURDIR)/scripts
RUN_DIR            := $(CURDIR)/run
DATA_DIR           := $(CURDIR)/data
DATA_CORPUS_DIR    := $(CURDIR)/data-corpus
DATA_CORPUS_FILE   := $(CURDIR)/data-corpus.tar.xz

# variables
MERGE_CSV := $(RUNR_DIR)/inst/merge-csv.R
ON_EACH_PACKAGE := $(MAKE) on-each-package

# runs
CONSOLIDATE_TYPES_CSV := $(RUN_DIR)/consolidate-types/types.csv
PACKAGE_ASSERTS_CSV   := $(RUN_DIR)/package-asserts/asserts.csv
PACKAGE_FUNCTIONS_CSV := $(RUN_DIR)/package-functions/functions.csv
PACKAGE_REVDEPS_FILE  := $(RUN_DIR)/package-revdeps/revdeps.txt
PROPAGATR_CODE_CSV    := $(RUN_DIR)/propagatr-code/runnable-code.csv
PROPAGATR_RUN_CSV     := $(RUN_DIR)/propagatr-run/run.csv
REVDEPS_CODE_CSV      := $(RUN_DIR)/revdeps-code/runnable-code.csv
REVDEPS_RUN_CSV       := $(RUN_DIR)/revdeps-run/run.csv

# data
ASSERTIONS_ALL_CSV        := $(DATA_DIR)/assertions-all.csv
ASSERTIONS_FAILED_CSV     := $(DATA_DIR)/assertions-failed.csv
TYPEDECLARATION           := $(DATA_DIR)/TYPEDECLARATION
TYPE_TRACES_CSV           := $(DATA_DIR)/type-traces.csv
TYPE_ANALYSIS_DIR         := $(DATA_DIR)/type-analysis
TYPE_ANALYSIS_PACKAGE_DIR := $(TYPE_ANALYSIS_DIR)/packages
TYPE_ANALYSIS_MERGED_FILE := $(TYPE_ANALYSIS_DIR)/merged.csv

TYPE_ANALYZER         := type-analyzer/type-analyzer

# notebooks
NOTEBOOKS_DIR           := notebooks
EVALUATION_DIR          := evaluation
EVALUATION_HTML         := $(EVALUATION_DIR)/evaluation.html
EVALUATION_ASSERTS_HTML := $(EVALUATION_DIR)/evaluation-asserts.html
CORPUS_ANALYSIS_HTML    := $(EVALUATION_DIR)/corpus-analysis.html

NOTEBOOKS := $(EVALUATION_HTML) $(PACKAGE_ASSERTS_HTML) $(CORPUS_ANALYSIS_HTML)

.PHONY: \
  on-each-package \
  notebooks \
  consolidate-types \
  package-asserts \
  package-functions \
  package-revdeps \
  propagatr-code \
  propagatr-run \
  revdeps-code \
  revdeps-run \
  type-analyzer \
  .compress-data-corpus

all: $(DATA_DIR) $(EVALUATION_HTML)

clean:
	-rm -fr $(DATA_DIR)
	-rm -fr $(RUN_DIR)

$(EVALUATION_HTML): $(DATA_DIR)
$(EVALUATION_HTML): export DATA_DIR=$(DATA_CORPUS_DIR)
$(EVALUATION_ASSERTS_HTML): $(DATA_CORPUS_DIR)/cran/cran-asserts.csv
$(EVALUATION_ASSERTS_HTML): export DATA_DIR=$(DATA_CORPUS_DIR)
$(CORPUS_ANALYSIS_HTML): $(DATA_CORPUS_DIR)/cran/cran-details.csv
$(CORPUS_ANALYSIS_HTML): export DATA_DIR=$(DATA_CORPUS_DIR)

$(EVALUATION_DIR)/%.html : $(NOTEBOOKS_DIR)/%.Rmd
	-mkdir -p $(EVALUATION_DIR)
	$(RUNR_DIR)/inst/start-xvfb.sh
	R --slave -q -e 'rmarkdown::render("$(NOTEBOOKS_DIR)/$(<F)", output_dir="$(EVALUATION_DIR)", params=list(data_dir="$(realpath $(DATA_DIR))", scripts_dir="$(realpath $(SCRIPTS_DIR))", output_dir="$(realpath $(EVALUATION_DIR))"))'

$(TYPE_ANALYZER):
	$(MAKE) -C type-analyzer

$(PACKAGE_FUNCTIONS_CSV): export OUTPUT_DIR=$(@D)
$(PACKAGE_FUNCTIONS_CSV):
	$(ON_EACH_PACKAGE) TASK=$(RUNR_TASKS_DIR)/package-functions.R
	$(MERGE_CSV) "$(OUTPUT_DIR)" $(@F)

$(PACKAGE_ASSERTS_CSV): export OUTPUT_DIR=$(@D)
$(PACKAGE_ASSERTS_CSV):
	$(ON_EACH_PACKAGE) TASK=$(RUNR_TASKS_DIR)/package-asserts.R
	$(MERGE_CSV) "$(OUTPUT_DIR)" $(@F)

$(PACKAGE_REVDEPS_FILE): export OUTPUT_DIR=$(@D)
$(PACKAGE_REVDEPS_FILE):
	$(ON_EACH_PACKAGE) TASK=$(RUNR_TASKS_DIR)/package-revdeps.R
	$(MERGE_CSV) "$(OUTPUT_DIR)" $(basename $(@F)).csv
	R --slave -q -e 'writeLines(unique(readr::read_csv("$(basename $@).csv")$$revdep), "$(PACKAGE_REVDEPS_FILE)")'

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

	-rm -fr $(TYPEDECLARATION)
	-mkdir -p $(TYPEDECLARATION)
	cat $(PACKAGES_FILE) | parallel cp -v "$(@D)/{1}/{1}" $(TYPEDECLARATION)

	$(MERGE_CSV) "$(OUTPUT_DIR)" $(@F)

$(TYPEDECLARATION): $(CONSOLIDATE_TYPES_CSV)

$(TYPE_TRACES_CSV): $(CONSOLIDATE_TYPES_CSV)
	cp $(CONSOLIDATE_TYPES_CSV) $(TYPE_TRACES_CSV)

$(ASSERTIONS_ALL_CSV): $(REVDEPS_RUN_CSV)
	cp $(dir $(REVDEPS_RUN_CSV))/$(@F) $(ASSERTIONS_ALL_CSV)

$(ASSERTIONS_FAILED_CSV): $(REVDEPS_RUN_CSV)
	cp $(dir $(REVDEPS_RUN_CSV))/$(@F) $(ASSERTIONS_FAILED_CSV)

$(REVDEPS_CODE_CSV): $(PACKAGE_REVDEPS_FILE)
$(REVDEPS_CODE_CSV): export OUTPUT_DIR=$(@D)
$(REVDEPS_CODE_CSV):
	$(ON_EACH_PACKAGE) TASK=$(RUNR_TASKS_DIR)/package-runnable-code.R PACKAGES_FILE=$(PACKAGE_REVDEPS_FILE)
	$(MERGE_CSV) "$(OUTPUT_DIR)" $(@F) runnable-code-metadata.csv

$(REVDEPS_RUN_CSV): $(REVDEPS_CODE_CSV) $(CONSOLIDATE_TYPES_CSV)
$(REVDEPS_RUN_CSV): export OUTPUT_DIR=$(@D)
$(REVDEPS_RUN_CSV): export START_XVFB=1
$(REVDEPS_RUN_CSV): export CONTRACTR_TYPEDECLARATION=$(TYPEDECLARATION)
$(REVDEPS_RUN_CSV):
	$(ON_EACH_PACKAGE) TASK=$(CURDIR)/scripts/run-extracted-code-contractr.R \
    PACKAGES_FILE=$(PACKAGE_REVDEPS_FILE) \
    ARGS="$(dir $(REVDEPS_CODE_CSV))/{1/}"
	$(MERGE_CSV) "$(OUTPUT_DIR)" $(@F) $(notdir $(ASSERTIONS_ALL_CSV)) $(notdir $(ASSERTIONS_FAILED_CSV))

$(TYPE_ANALYSIS_MERGED_FILE): $(TYPE_ANALYZER) $(TYPEDECLARATION)
	-rm -fr $(TYPE_ANALYSIS_PACKAGE_DIR)
	-mkdir -p $(TYPE_ANALYSIS_PACKAGE_DIR)

	@echo "Analyzing types from $(TYPEDECLARATION) into $(TYPE_ANALYSIS_PACKAGE_DIR) ..."
	for file in $(TYPEDECLARATION)/*; do \
	    echo "Analyzing '$$file'"; \
	    $(TYPE_ANALYZER) $$file $(TYPE_ANALYSIS_PACKAGE_DIR)/`basename $$file`.csv; \
	done

	@echo "Merging package type analysis files into $(TYPE_ANALYSIS_MERGED_FILE) ..."
	R --slave -q -e "readr::write_csv(do.call(rbind, lapply(Sys.glob('$(TYPE_ANALYSIS_PACKAGE_DIR)/*.csv'), readr::read_csv)), '$(TYPE_ANALYSIS_MERGED_FILE)')"

data: $(TYPE_TRACES_CSV) \
  $(TYPEDECLARATION) \
  $(ASSERTIONS_ALL_CSV) \
  $(ASSERTIONS_FAILED_CSV) \
  $(TYPE_ANALYSIS_MERGED_FILE)

corpus-analysis:    $(CORPUS_ANALYSIS_HTML)
evaluation-asserts: $(EVALUATION_ASSERTS_HTML)
evaluation:         $(EVALUATION_HTML)

consolidate-types: $(CONSOLIDATE_TYPES_CSV)
package-asserts:   $(PACKAGE_ASSERTS_CSV)
package-functions: $(PACKAGE_FUNCTIONS_CSV)
package-revdeps:   $(PACKAGE_REVDEPS_FILE)
propagatr-code:    $(PROPAGATR_CODE_CSV)
propagatr-run:     $(PROPAGATR_RUN_CSV)
revdeps-code:      $(REVDEPS_CODE_CSV)
revdeps-run:       $(REVDEPS_RUN_CSV)
type-analyzer:     $(TYPE_ANALYZER)
type-analysis:     $(TYPE_ANALYSIS_MERGED_FILE)

on-each-package:
	@[ "$(TASK)" ] || ( echo "*** Undefined TASK"; exit 1 )
	@[ -x "$(TASK)" ] || ( echo "*** $(TASK): no such file"; exit 1 )
	@[ "$(OUTPUT_DIR)" ] || ( echo "*** Undefined OUTPUT_DIR"; exit 1 )
	@[ -z "$(START_XVFB)" ] || $(RUNR_DIR)/inst/start-xvfb.sh

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

$(DATA_CORPUS_DIR): $(DATA_CORPUS_FILE)
	tar xfvJ $(DATA_CORPUS_FILE)

.compress-data-corpus:
	tar cfvJ $(DATA_CORPUS_FILE) $(DATA_CORPUS_DIR)
