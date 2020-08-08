CXX         := g++
CXXFLAGS    := --std=c++14 -O2 -ggdb3
CXX_SRC_DIR := src

TASTR_DIR               := $(R_PROJECT_BASE_DIR)/tastr
TASTR_INCLUDE_PATH      := $(TASTR_DIR)/build/include
TASTR_LIBRARY_PATH      := $(TASTR_DIR)/build/lib
TYPE_ANALYZER           := type-analyzer
TYPE_ANALYZER_SRC_FILES := $(shell find $(CXX_SRC_DIR) -name "*.cpp") $(shell find $(CXX_SRC_DIR) -name "*.hpp")

$(TYPE_ANALYZER): $(TYPE_ANALYZER_SRC_FILES)
	$(CXX) $(CXXFLAGS) -I$(TASTR_INCLUDE_PATH) -I$(TASTR_INCLUDE_PATH)/tastr $^ $(TASTR_LIBRARY_PATH)/libtastr.a -o$@

clean:
	-$(RM) -f $(TYPE_ANALYZER)
