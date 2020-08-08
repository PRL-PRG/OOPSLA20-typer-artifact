## all CRAN packages
CRAN_PACKAGES_DETAILS   <- path(params$data_dir, "cran", "cran-details.csv")
CRAN_REVDEPS            <- path(params$data_dir, "cran", "cran-revdeps.csv")
CRAN_ASSERTS            <- path(params$data_dir, "cran", "cran-asserts.csv")

## corpus only
CORPUS_FILE                  <- path(params$output_dir, "corpus.txt")
CORPUS_DETAILS               <- path(params$output_dir, "corpus-details.csv")
CORPUS_REVDEPS               <- path(params$output_dir, "corpus-revdeps.csv")
CORPUS_REVDEPS_UNIQUE        <- path(params$output_dir, "corpus-revdeps.txt")
COVERAGE_REVDEP_FUNCTIONS    <- path(params$output_dir, "corpus-coverage-functions.csv")

## plots
CORPUS_PLOT           <- path(params$output_dir, "corpus.pdf")

## asserts
PACKAGE_ASSERTS       <- path(params$data_dir, "package-asserts.csv")

TAGS_DIR <- path(params$output_dir, ".")
