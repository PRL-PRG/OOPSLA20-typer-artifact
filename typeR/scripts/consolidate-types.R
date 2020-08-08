#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(purrr))
library(readr)
library(runr)
library(tidyr)

base_dir <- dirname(dirname(runr::current_script()))

source(file.path(base_dir, "scripts", "data-analysis-lib.R"))

# args
args <- commandArgs(trailingOnly=T)

pname <- basename(args[1])
src_dir <- args[2]
package_funs_dir <- args[3]
tgt_path <- getwd()
types_path <- getwd()

# First, get the list of functions that we care about, for the package in question.
package_funs_file <- file.path(package_funs_dir, pname, "functions.csv")
package_funs <- read_csv(package_funs_file)
package_merge_df <-
  transmute(package_funs, package=pname, fun_name=fun)

# Also, we do this for stats, graphics, grDevices, utils, datasets, methods, Autoloads, and base.
## base_pkgs <- file.path(
##   package_funs_dir,
##   readLines(file.path(base_dir, "packages-base-r.txt")),
##   "functions.csv"
## )

## base_pkgs_funs <- lapply(base_pkgs, function(x) {
##   read_csv(x) %>% transmute(
##     package=basename(dirname(x)),
##     fun_name=fun
##   )
## })

## base_pkgs_funs_merged <- do.call(bind_rows, base_pkgs_funs)
## package_merge_df <- bind_rows(package_merge_df, base_pkgs_funs_merged)


# get all relevant files
lof <- list.files(paste(src_dir, pname, sep="/"), recursive=T, full.names=T) 

# get only files pointing to traces
lof <- lof[grepl("traces_", lof)]

# the ccccddcli is to account for cases where the trace_hash randomly parses as a character, causing issues
# in the bind
big_df <- map(lof, function(f) {
    # Get the DF.
    df <- read_csv(f, col_types=paste0("ccccddcli", paste0(rep("c", 1000), collapse="")))
    # Get function IDs we want: IDs corresponding to top-level functions, and closures in
    # internal packages.
    fids_to_grab <- df %>% 
        merge(package_merge_df, by = c("package", "fun_name")) %>% 
        as_tibble %>%
        select(fun_id) %>% unique %>% unlist %>% unname
    # Get only the entries matching those IDs.
    df %>% filter(fun_id %in% fids_to_grab)
}) %>% bind_rows

# Next, reduce to X args.
# By default, X is 20.
big_df <- big_df %>% quick_trim_df_to_x_args

# filter our long jumps
big_df <- filter(big_df, arg_t_r != "jumped")

# Finally, simplify, and write out.
big_df <- big_df %>% quick_simplify_types %>% 
          group_by_at(vars(-trace_hash, -type_hash, -count)) %>% 
          summarize(count=sum(count)) %>%
          ungroup

# Write it.
write_csv(big_df, file.path(tgt_path, "types.csv"))

# Also, get the types while we're at it.
# Types for base and other internal packages will be determined later,
# by binding a mega data frame.
#   structless: do we want to ignore structs?
#   tupleless: do we want to ignore tuples?
#   restrict_analysis: do we only want to type based on the package under analysis?
#                      (this is irrelevant now I think, with the way this is organized)

# Might be how to do this:
# commandArgs <- function(...) c(pname, tgt_path, types_path)

types <- get_fun_types_for_package(big_df, pname, structless=TRUE, tupleless=TRUE, restrict_analysis=TRUE, unify_lists=FALSE, exported_funs = package_merge_df %>% filter(package == pname))

if (!dir.exists(types_path)) dir.create(types_path, recursive=TRUE)

writeLines(format_package_types_for_write(types), paste0(types_path, "/", pname))
