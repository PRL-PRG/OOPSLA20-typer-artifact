#!/usr/bin/env Rscript
options(repos=Sys.getenv("CRAN_LOCAL_MIRROR", "https://cloud.r-project.org"))

# pins the current version of the corpus packages as well as
# their dependencies

BASE_PACKAGES <- c(
  "base",
  "compiler",
  "datasets",
  "graphics",
  "grDevices",
  "grid",
  "methods",
  "parallel",
  "splines",
  "stats",
  "stats4",
  "tcltk",
  "tools",
  "utils"
)

packages_corpus <- readLines("../typeR/packages-corpus.txt")
packages_base <- readLines("our-packages-dependencies.txt")
packages <- unique(c(packages_corpus, packages_base))

dependencies <- unlist(tools::package_dependencies(packages, recursive=T))
packages <- unique(c(packages, dependencies))
packages <- sort(setdiff(packages, BASE_PACKAGES))

versions <- sapply(packages, function(x) packageDescription(x)$Version)

df <- data.frame(packages, versions)
write.table(df, "CRAN-packages.txt", row.names=FALSE, col.names=FALSE, quote=FALSE)
