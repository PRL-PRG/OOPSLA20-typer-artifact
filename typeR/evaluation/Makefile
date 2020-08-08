MKDIR := mkdir -p
RM := rm
R := R --slave -q
CXX := g++

CXXFLAGS := --std=c++17 -O2 -ggdb3

R_SRC_DIR   := R
CXX_SRC_DIR := src
INST_DIR    := inst
DATA_DIR    := data
HTML_DIR    := html

TYPE_ANALYSIS_DIR         := $(DATA_DIR)/type-analysis
TYPE_ANALYSIS_PACKAGE_DIR := $(TYPE_ANALYSIS_DIR)/packages
TYPE_ANALYSIS_MERGED_FILE := $(TYPE_ANALYSIS_DIR)/merged.csv

TYPE_ANALYSIS_EVALUATION_NOTEBOOK := $(R_SRC_DIR)/evaluate-type-analysis.Rmd
TYPE_ANALYSIS_EVALUATION_DOCUMENT := $(HTML_DIR)/evaluate-type-analysis.html

RUNR_DIR           := $(R_PROJECT_BASE_DIR)/runr
TASTR_DIR          := $(R_PROJECT_BASE_DIR)/tastr
TASTR_INCLUDE_PATH := $(TASTR_DIR)/build/include
TASTR_LIBRARY_PATH := $(TASTR_DIR)/build/lib

TYPE_ANALYZER           := $(INST_DIR)/type-analyzer
TYPE_ANALYZER_SRC_FILES := $(shell find $(CXX_SRC_DIR) -name "*.cpp") $(shell find $(CXX_SRC_DIR) -name "*.hpp")

TYPEDECLARATION_DIR := $(shell $(R) -e "cat(system.file('TYPEDECLARATION', package='contractr'))")

.PHONY: \
  all \
  analyze-package-types \
  evaluate-type-analysis \
  clean

all: evaluate-type-analysis

$(TYPE_ANALYZER): $(TYPE_ANALYZER_SRC_FILES)
	-$(MKDIR) $(INST_DIR)
	$(CXX) $(CXXFLAGS) -I$(TASTR_INCLUDE_PATH) -I$(TASTR_INCLUDE_PATH)/tastr $^ $(TASTR_LIBRARY_PATH)/libtastr.a -o$@

clean:
	-$(RM) -f $(TYPE_ANALYZER)
	-$(RM) -fr $(TYPE_ANALYSIS_DIR)

analyze-package-types: $(TYPE_ANALYZER)
	-$(RM) -fr $(TYPE_ANALYSIS_PACKAGE_DIR)
	-$(MKDIR) $(TYPE_ANALYSIS_PACKAGE_DIR)

	@echo "Analyzing types from $(TYPEDECLARATION_DIR) into $(TYPE_ANALYSIS_PACKAGE_DIR) ..."
	@for file in $(TYPEDECLARATION_DIR)/*; do \
	    echo "Analyzing '$$file'"; \
	    $(TYPE_ANALYZER) $$file $(TYPE_ANALYSIS_PACKAGE_DIR)/`basename $$file`.csv; \
	done

	@echo "Merging package type analysis files into $(TYPE_ANALYSIS_MERGED_FILE) ..."
	$(R) -e "readr::write_csv(do.call(rbind, lapply(Sys.glob('$(TYPE_ANALYSIS_PACKAGE_DIR)/*.csv'), readr::read_csv)), '$(TYPE_ANALYSIS_MERGED_FILE)')"

evaluate-type-analysis: analyze-package-types
	$(MKDIR) $(HTML_DIR)
	$(R) --slave -q -e "rmarkdown::render('$(shell realpath $(TYPE_ANALYSIS_EVALUATION_NOTEBOOK))', output_dir='$(shell realpath $(HTML_DIR))', output_file='$(shell basename $(TYPE_ANALYSIS_EVALUATION_DOCUMENT))', params=list(type_analysis_datafile = '$(shell realpath $(TYPE_ANALYSIS_MERGED_FILE))'))"
