R_PROJECT_BASE_DIR <- "/R"
R_VERSION <- "3.5"
TYPER_BASE_DIR <- "~/typeR"

EVALUATION_DIR <- path(TYPER_BASE_DIR, "evaluation")
PAPER_DIR      <- path(TYPER_BASE_DIR, "paper")
PLOT_DIR       <- path(PAPER_DIR, "plots")
RUN_DIR        <- path(TYPER_BASE_DIR, "run")
TAGS_DIR       <- PAPER_DIR

fs::dir_create(
  c(EVALUATION_DIR, PAPER_DIR, PLOT_DIR, RUN_DIR, TAGS_DIR), 
  recurse=TRUE
)

## all CRAN packages
CRAN_PACKAGES_DETAILS   <- path(EVALUATION_DIR, "cran-details.csv")
CRAN_REVDEPS            <- path(EVALUATION_DIR, "cran-revdeps.csv")
CRAN_ASSERTS            <- path(EVALUATION_DIR, "cran-asserts.csv")

## corpus only
CORPUS_FILE                  <- path(EVALUATION_DIR, "corpus.txt")
CORPUS_DETAILS               <- path(EVALUATION_DIR, "corpus-details.csv")
CORPUS_REVDEPS               <- path(EVALUATION_DIR, "corpus-revdeps.csv")
CORPUS_REVDEPS_UNIQUE        <- path(EVALUATION_DIR, "corpus-revdeps.txt")
COVERAGE_REVDEP_FUNCTIONS    <- path(EVALUATION_DIR, "corpus-coverage-functions.csv")

## plots
CORPUS_PLOT           <- path(PLOT_DIR, "corpus.pdf")

## asserts
PACKAGE_ASSERTS       <- path(RUN_DIR, "package-asserts", "package-asserts.csv")
