
require(tidyverse)

source("~/pldi/scripts/support_scripts/data_analysis_lib.R")

# args
args <- commandArgs(trailingOnly=T)
pname <- args[1]
src_dir <- args[2]
tgt_path <- args[3]
types_path <- args[4]

# First, get the list of functions that we care about, for the package in question.
package_funs <- read_csv(paste0("/var/lib/R/project-typeR/run/package-functions/", pname, "/functions.csv"))
package_merge_df <- tibble(package = rep(pname, nrow(package_funs)), fun_name = package_funs$fun)

# Also, we do this for stats, graphics, grDevices, utils, datasets, methods, Autoloads, and base.
base_funs <- read_csv("~/internal-package-exported-funs/base/functions.csv")
graphics_funs <- read_csv("~/internal-package-exported-funs/graphics/functions.csv")
grDevices_funs <- read_csv("~/internal-package-exported-funs/grDevices/functions.csv")
methods_funs <- read_csv("~/internal-package-exported-funs/methods/functions.csv")
stats_funs <- read_csv("~/internal-package-exported-funs/stats/functions.csv")
utils_funs <- read_csv("~/internal-package-exported-funs/utils/functions.csv")

package_merge_df <- bind_rows(package_merge_df, base_funs, graphics_funs, grDevices_funs, methods_funs, stats_funs, utils_funs)

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

# Finally, simplify, and write out.
big_df <- big_df %>% quick_simplify_types %>% 
          group_by_at(vars(-trace_hash, -type_hash, -count)) %>% 
          summarize(count=sum(count)) %>%
          ungroup

# Write it.
write_csv(big_df, paste(tgt_path, "/", pname, ".csv", sep=""))

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

f <- file(paste0(types_path, "/", pname))
writeLines(format_package_types_for_write(types), f)
close(f)