#!/usr/bin/env Rscript

# This script is meant to run on the server to
# generate the URLs for bootstrapping local
# CRAN mirror with the same package versions.

library(glue)
library(RCurl)
library(pbapply)

options(repos=Sys.getenv("CRAN_LOCAL_MIRROR"))
pboptions(type="txt")

Ncpus <- min(c(16, parallel::detectCores()))
BASE_PACKAGES <- c(
  "base",
  "boot",
  "class",
  "cluster",
  "codetools",
  "compiler",
  "datasets",
  "foreign",
  "graphics",
  "grDevices",
  "grid",
  "KernSmooth",
  "lattice",
  "MASS",
  "Matrix",
  "methods",
  "mgcv",
  "nlme",
  "nnet",
  "parallel",
  "rpart",
  "spatial",
  "splines",
  "stats",
  "stats4",
  "survival",
  "tcltk",
  "tools",
  "utils"
)

get_package_url <- function(package, version) {
  filename <- glue("{package}_{version}.tar.gz")
  default_url <- glue("https://cran.r-project.org/src/contrib/{filename}")
  archive_url <- glue("https://cran.r-project.org/src/contrib/00Archive/{package}/{filename}")

  if (url.exists(default_url)) {
    default_url
  } else if (url.exists(archive_url)) {
    archive_url
  } else {
    NA
  }
}

packages_corpus <- readLines("packages-corpus.txt")
packages_base <- readLines("packages-base.txt")
packages <- unique(c(packages_corpus, packages_base))

dependencies <- unlist(tools::package_dependencies(packages, recursive=T))
packages <- unique(c(packages, dependencies))
packages <- setdiff(packages, BASE_PACKAGES)

versions <- sapply(packages, function(x) packageDescription(x)$Version)

res <- pbsapply(
  seq_along(packages),
  function(idx) get_package_url(packages[idx], versions[idx]),
  cl=Ncpus
)

#tools::write_PACKAGES(local_cran, type="source", verbose=T)

missing_packages_lgl <- is.na(res)

if (any(missing_packages_lgl)) {
  cat("Missing packages: \n")
  cat(
    glue("{packages[missing_packages_lgl]}_{versions[missing_packages_lgl]}"),
    sep="\n"
  )
}

writeLines(res[!missing_packages_lgl], "CRAN-packages-urls.txt")
