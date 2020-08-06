#!/usr/bin/env Rscript
options(repos=Sys.getenv("CRAN_LOCAL_MIRROR", "https://cloud.r-project.org"))

# pins the current version of the corpus packages as well as
# their dependencies

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

packages_corpus <- readLines("typeR/packages-corpus.txt")
packages_base <- readLines("scripts/packages-base.txt")
packages <- unique(c(packages_corpus, packages_base))

dependencies <- unlist(tools::package_dependencies(packages, recursive=T))
packages <- unique(c(packages, dependencies))
packages <- sort(setdiff(packages, BASE_PACKAGES))

versions <- sapply(packages, function(x) packageDescription(x)$Version)

df <- data.frame(packages, versions)
write.table(df, "CRAN-packages.txt", row.names=FALSE, col.names=FALSE, quote=FALSE)
