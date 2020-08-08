
suppressPackageStartupMessages(library(dplyr))
library(readr)
suppressPackageStartupMessages(library(purrr))
require(stringr)    # Need for str_* methods.
library(tidyr)
 
#
# Constants
#
# Basic primitive type names
PRIM_TYPES <- c("integer", "double", "logical", "raw", "character", "complex")
# Preamble for .csv files
PREAMBLE <- c("package_being_analyzed","package","fun_name","fun_id","trace_hash","type_hash","dispatch","has_dots","count")
NUM_PREAMBLE <- length(PREAMBLE)
# Preamble types, for read_csv(...)
PREAMBLE_TYPES <- c("c", "c", "c", "c", "d", "d", "c", "l", "d") # Using compact representation
# Special case of preamble when type_hash is included, though I doubt this is necessary now.
PREAMBLE_WITH_TYPES_HASH <- c("package_being_analyzed", "package", "fun_name", "fun_id", "trace_hash", "types_hash", "count")

# Data analysis flags.
NAMES_REMOVED <- T # Do we remove list names?
TUPLE_TO_LIST_HEURSTIC <- 5 # Heuristic for turning tuple<a, b, c, ..., z> into list<...>
LIST_UNION_HEURISTIC <- 1000000 # When to turn list<A | B | C> into list<any>

# Probably useless now.
# TYPE_SYSTEM_T0 <- list(
#     "integer", "integer[]", "double", "double[]", "character", "character[]",
#     "complex", "complex[]", "logical", "logical[]", "raw", "raw[]", 
#     "!integer", "!integer[]", "!double", "!double[]", "!character", "!character[]",
#     "!complex", "!complex[]", "!logical", "!logical[]", "!raw", "!raw[]",
#     "list<T>", "struct{n:T}", "expression", "environment"
# )

BASE_DATA_PRIMITIVE_NAMES <- c("integer", "double", "complex", "logical", "raw", "character")

# This is old, and not used in the current pipeline.
# convert_raw_data_type_to_T0 <- function(t) {
#     # First, isolate the tags and dimensions.
#     # Note: If there are no dims or no tags, the entry in the result will be NULL.
#     #       Is that something that we want to fix?
#     type_and_tags <- strsplit(t, split="@", fixed=T)[[1]]
#     tags <- c()
#     if (length(type_and_tags) > 1)
#         tags <- type_and_tags[2:length(type_and_tags)]
#     type_and_dims <- strsplit(type_and_tags[1], split="[", fixed=T)[[1]]
#     dims <- c()
#     if (length(type_and_dims) > 1){
#         dims <- type_and_dims[2]
#         # Remove the "]" at the end.
#         dims <- substr(dims, 1, nchar(dims) - 1)
#     }
#     base_type <- type_and_dims[1]
#     # process_me <- list(base_type = base_type, dims = dims, tags = tags)
#     # Now, process the type to a T0 type. TODO the above seems broadly useful, factor
#     # into own function.
#     # Notable hurdles:
#     # 1. dealing with the names tag
#     # Get the primitive cases first.
#     if (base_type %in% BASE_DATA_PRIMITIVE_NAMES) {
#         # Figure out if it's a scalar, or a vector.
#         # dim will not be NULL.
#         # First, if NA-free is a tag, want to put a ! in the type.
#         na_free_str <- ""
#         if ("NA-free" %in% tags)
#             na_free_str <- "!"
#         # Deal with matrix. In T0, matrices are just vectors.
#         if (grepl("-", dims, fixed=T))
#             return(paste0(na_free_str, base_type, "[]"))
#         dim_as_int <- dims %>% as.integer
#         if (dim_as_int == 1) {
#             return(paste0(na_free_str, base_type))
#         } else if (dim_as_int > 1) {
#             return(paste0(na_free_str, base_type, "[]"))
#         } else {
#             # dim_as_int == 0, this is a NULL of the primitive type.
#             return(paste0("NULL<", base_type, ">"))
#         }
#     } else if ("list" == substr(base_type, 1, 4)) {
#         # Figure out if it's a struct, or a normal list.
#         # TODO: We might need a heuristic here... Because sometimes people have lists
#         #       with names that aren't structs.
#         # First, figure out if it's NULL-free.
#         null_free_str <- ""
#         if ("NULL-free" %in% tags)
#             null_free_str <- "!"
#         # Deal with names.
#         # Finally, deal with the inner type.
#         # Right now, the inner type is more simple than it could be (we only get surface-level
#         # information for these), and might be fine to output as-is. TODO Double check.
#         paste0(null_free_str, base_type)
#     } else {
#         # In the default case, just split out the base type.
#         # This catches cases like environment, language, etc.
#         paste0(base_type)
#     }
# }

# Old.
# preprocess_file_build_header <- function(file_name) {
#     # get lines from file
#     fc <- file(file_name)
#     lines <- readLines(fc)
#     close(fc)

#     # split all by ", "
#     lapply(lines, function(x) strsplit(x, split=",")) %>%
#     lapply(unlist) %>% 
#     sapply(length) -> lengths

#     max(lengths) -> max_len

#     max_num_args <- (max_len - NUM_PREAMBLE) / 3

#     # build new row for beginning
#     new_first_row <- paste(PREAMBLE, collapse=",")
#     new_first_row <- c(
#         new_first_row, 
#         c("arg_t_r", "arg_c_r", "arg_a_r"),
#         mapply(function(x, y) paste(x, y, sep=""), 
#                rep(c("arg_t", "arg_c", "arg_a"), max_num_args - 1), 
#                rep(seq_len(max_num_args - 1) - 1, each=3))) %>%
#     (function(x) {paste(x, collapse=",")})
    
#     # note: -1, return, is first

#     fc <- file(file_name)
#     writeLines(c(new_first_row, lines), fc)
#     close(fc)

#     sum((lengths - NUM_PREAMBLE) %% 3) == 0
# }

# good pattern: "(?<=\\{).*?(?=\\})"
# Command:
#
# read_csv(...) %>% replace_NAs %>% df_remove_curlies %>% df_strings_to_lists %>% df_collapse_by_fn_id
# (and %>% df_strings_to_lists(start_ind=2) if you want lists)
# generate_init_dependency_file <- function(file_name) {
#     read_csv(file_name) %>%
#     replace_NAs %>%
#     df_remove_curlies %>%
#     df_strings_to_lists %>%
#     df_collapse_by_fn_id %>%
#     df_strings_to_lists(start_ind = 4) %>%
#     df_to_list_of_dep_inits -> write_me

#     write_me
#     # TODO: generate the outfile name?
# }

# replace NAs as appropriate:
# for types, just put ""
# for classes and attrs, put "{}"
# replace_NAs <- function(df) {
#     num_args_to_gen <- (ncol(df) - NUM_PREAMBLE) / 3 - 1

#     dyn_col_names <- c(
#         c("arg_t_r", "arg_c_r", "arg_a_r"), 
#         mapply( function(x, y) paste(x, y, sep=""), 
#         rep(c("arg_t", "arg_c", "arg_a"), num_args_to_gen), 
#         rep(seq_len(num_args_to_gen), each=3) - 1) %>% unname)

#     df_r <- df

#     type_col_names <- dyn_col_names[seq(1, length(dyn_col_names), by=3)]
#     other_col_names <- setdiff(dyn_col_names, type_col_names)

#     names(type_col_names) <- type_col_names
#     names(other_col_names) <- other_col_names

#     replace_na(df_r, lapply(type_col_names, function(x) "")) %>%
#     replace_na(lapply(other_col_names, function(x) "{}"))
# }

# df_remove_curlies <- function(df) {
#     df_r <- df

#     extract_list <- names(df)
#     extract_list <- extract_list[(NUM_PREAMBLE + 1):length(extract_list)]
#     extract_list <- extract_list[setdiff(seq_len(length(extract_list)), seq(1, length(extract_list), 3))]

#     for (i in 1:length(extract_list)) {
#         # note: need !!sym(col_names[i]) for dumb shite
#         df_r %>% mutate(!!extract_list[i] := str_extract(!!sym(extract_list[i]), "(?<=\\{).*?(?=\\})")) -> df_r
#     }

#     df_r # return
# }

# df_strings_to_lists <- function(df, start_ind = NUM_PREAMBLE + 1) {
#     # first, remove the {}s
#     df_r <- df

#     extract_list <- names(df)
#     extract_list <- extract_list[start_ind:length(extract_list)]

#     for (i in 1:length(extract_list)) {
#         # while we're here... lets turn the column into a list
#         df_r %>% mutate(!!extract_list[i] := strsplit(!!sym(extract_list[i]), split=",")) -> df_r 
#     }

#     df_r # return
# }

# this one is very slow
# this is the function that will give you the collapsed tibble
# df_collapse_by_fn_id <- function(df) {
#     df_r <- df

#     extract_list <- names(df)
#     extract_list <- extract_list[(NUM_PREAMBLE + 1):length(extract_list)]

#     # TODO: we lose the counts here...
#     df_r %<>% group_by(package, fun_name, fun_id) %>%
#               summarize_at(extract_list, function(x) {
#                 paste(union(x, c()), collapse=",")
#               })

#     # unfortunately, there are character(0) strings inside the results.
#     # lets get rid of them
#     df_r %<>% rowwise %>% mutate_at(extract_list, function(x) {

#         # dunno if removing NAs make sense here, b/c it signifies that
#         # those arguments were not present
#         x_s <- strsplit(x, split="character\\(0\\)") %>%
#         unlist %>%
#         paste(collapse="") %>%
#         str_replace(pattern=",,", replacement=",")

#         if (substr(x_s, 1, 1) == ",")
#             x_s <- substr(x_s, 2, nchar(x_s))
#         else if (substr(x_s, nchar(x_s), nchar(x_s)) == ",")
#             x_s <- substr(x_s, 1, nchar(x_s)-1)

#         x_s
#     })

#     df_r
# }

#
#

# df_to_list_of_dep_inits <- function(df, start_index = NUM_PREAMBLE) {
    
#     inds_to_search <- seq(start_index, ncol(df), 3)
#     r <- apply(df, 1, function(row) {
#         fn_id <- row["fun_id"]
#         types <- row[inds_to_search]
#         pposes <- -1:(length(inds_to_search)-2)

#         mapply(function(ppos, type) {
#             if (length(type) > 0)
#                 paste(paste(fn_id, ppos, sep=","), paste(type, collapse=","), sep=" : ")
#             else
#                 ""
#         }, pposes, types) -> the_strings

#         build_me <- paste(the_strings[nchar(the_strings) != 0], collapse="\n")
#     }) %>% paste(collapse="\n")
    
#     r 
# }

#
# main function
# call from scripts
#
# gen_init_deps_for_package <- function(pkg, path_to_res = "results") {

#     path_into_pkg <- paste(path_to_res, pkg, sep="/")

#     type_of_files_analyzed <- list.files(path_into_pkg, full.names=T)

#     sapply(type_of_files_analyzed, function(path) {
#         files_for_type <- list.files(path)
        
#         trace_files <- files_for_type[sapply(files_for_type, function(x) grepl("traces_", x, fixed=T))]

#         write_us <- sapply(paste(path, trace_files, sep="/"), generate_init_dependency_file)

#         # write the contents
#         # generate names to write to
#         # dependency_init_FILE.ext
#         write_out_names <- strsplit(trace_files, split="_") %>% sapply(function(l) {
#             paste("dependency_init_", l[2:length(l)], sep="")
#         })

#         mapply(function(out_file_name, write_out_data) {
#             fc <- file(paste(path, out_file_name, sep="/"))
#             writeLines(write_out_data, fc)
#             close(fc)
#         }, write_out_names, write_us)
#     })

# }

# generate string of signatures to be written out to file
# generate_all_signatures <- function(df) {
#     apply(df, 1, function(row) {
#         smaller_row <- row[c(-1, -2, -4, -5)] # TODO hardcoded
#         smaller_row <- smaller_row[!sapply(smaller_row, is.na)]
#         lapply(smaller_row, function(el) {
#             el
#         }) %>% paste(collapse=",")
#     }) %>% paste(collapse="\n")
# }

# grab all files and get base package functions
# get_all_base_in_dir <- function(dir) {

#     all_files <- list.files(dir, full.names=T, recursive=T)

#     trace_files <- all_files[grep("traces_", all_files, fixed=T)]

#     # read_csv
#     Reduce(function(x, y) {
#         bound <- bind_rows(read_csv(y), x)
#         bound %>%
#         filter(package=="base") %>%
#         group_by_at(setdiff(names(bound), "count")) %>%
#         summarize(count=sum(count)) %>%
#         ungroup
#     }, trace_files, data.frame())
#     # get base funs
#     # bind 
#     # group and summarize

# }

# To be performed on rows of a data frame
# old_check_sig_equality_for_dispatch <- function(row1, row2) {
#     names_to_check <- setdiff(names(row1), PREAMBLE)

#     (row1 %>% select(names_to_check) == row2 %>% select(names_to_check)) %>% reduce(`&&`)
# }

# old_remove_dispatch <- function(df, dispatch_name, base_name) {

#     names_to_check <- setdiff(names(df), PREAMBLE)

#     dispatch_rows <- df %>% filter(fun_name == dispatch_name)
#     dispatch_strs <- dispatch_rows %>% apply(1, function(r) paste(r[names_to_check], collapse=","))

#     base_rows <- df %>% filter(fun_name == base_name)
#     base_strs <- base_rows %>% apply(1, function(r) paste(r[names_to_check], collapse=","))

#     # now, we want to uncount the dispatch cases
#     # note: we can use the trace_hash to find the rows that we're interested in
#     base_rows_of_interest <- base_rows[base_strs %in% dispatch_strs, ]
#     # dispatch_rows_of_interest <- dispatch_rows[dispatch_strs %in% base_strs, ]

#     # line them up...
#     # this needs to be done with this stupid string hack...
#     base_rows_of_interest <- base_rows_of_interest[order(base_strs[base_strs %in% dispatch_strs]),]
#     # dispatch_rows_of_interest <- dispatch_rows_of_interest[order(dispatch_strs[dispatch_strs %in% base_strs]),]

#     # modify row counts << IN df >>, subtracting the dispatch #s from the base #s
#     all_strs <- df %>% apply(1, function(r) paste(r[setdiff(names(r), "count")], collapse=","))
#     base_strs_but_with_more <- base_rows_of_interest %>% apply(1, function(r) paste(r[setdiff(names(r), "count")], collapse=","))
    
#     # most awful line of code ever written
#     df[all_strs %in% base_strs_but_with_more, ][order(all_strs[all_strs %in% base_strs_but_with_more]),]$count <- 
#     df[all_strs %in% base_strs_but_with_more, ][order(all_strs[all_strs %in% base_strs_but_with_more]),]$count -
#     base_rows_of_interest[base_strs_but_with_more %in% all_strs,][order(base_strs_but_with_more[base_strs_but_with_more %in% all_strs]),]$count

#     # remove the now 0-count rows
#     df %>% filter(count > 0)
# }

# TODO: copy paste basically of above, except the beginning is slightly different
# old_remove_fun_redefs <- function(df, target_package, og_package, the_fun_name) {
#     names_to_check <- setdiff(names(df), PREAMBLE)

#     dispatch_rows <- df %>% filter(fun_name == the_fun_name, package == target_package)
#     dispatch_strs <- dispatch_rows %>% apply(1, function(r) paste(r[names_to_check], collapse=","))

#     base_rows <- df %>% filter(fun_name == the_fun_name)
#     base_strs <- base_rows %>% apply(1, function(r) paste(r[names_to_check], collapse=","))

#     base_rows_of_interest <- base_rows[base_strs %in% dispatch_strs, ]
#     base_rows_of_interest <- base_rows_of_interest[order(base_strs[base_strs %in% dispatch_strs]),]

#     all_strs <- df %>% apply(1, function(r) paste(r[setdiff(names(r), "count")], collapse=","))
#     base_strs_but_with_more <- base_rows_of_interest %>% apply(1, function(r) paste(r[setdiff(names(r), "count")], collapse=","))

#     df[all_strs %in% base_strs_but_with_more, ][order(all_strs[all_strs %in% base_strs_but_with_more]),]$count <- 
#     df[all_strs %in% base_strs_but_with_more, ][order(all_strs[all_strs %in% base_strs_but_with_more]),]$count -
#     base_rows_of_interest[base_strs_but_with_more %in% all_strs,][order(base_strs_but_with_more[base_strs_but_with_more %in% all_strs]),]$count

#     df %>% filter(count > 0)
# }

# TODO do we want pkg?
# TODO: I think we can run this in parallel, before combining. Think abt this.
# remove_dispatch_for_fun <- function(df, fun) { #, pkg="base") {
#     # sanitize fun for regex
#     unames <- df %>% select(fun_name) %>% unique
#     S3_dispatch_names <- unames[unames %>% map(function(e) grepl(pattern = paste(fun, ".", sep=""), e, fixed=T)) %>% unlist %>% unname, ] %>% unlist %>% unname

#     dispatch_rows <- df %>% filter(fun_name %in% S3_dispatch_names)
#     og_rows <- df %>% filter(fun_name == fun)

#     # TODO: for each og_row, remove matching dispatch row
#     candidates_for_purge <- intersect(og_rows$types_hash, dispatch_rows$types_hash)

#     # Want to remove all rows in og_rows with types_hash in candidates_for_purge.
#     df %>% anti_join(
#         df %>% filter(fun_name == fun) %>% filter(types_hash %in% candidates_for_purge),
#         by = "types_hash"
#     )
# }

# quick_get_named_primitives <- function(df) {
#     df %>% filter(str_detect(arg_t0, 'integer|double|complex|raw|logical|character')) %>% 
#     filter(!str_detect(arg_t0, 'list|frame')) %>% 
#     filter(str_detect(arg_t0, 'names'))
# }

# quick_pred_is_S3_dispatch <- function(df) {
#     # Is there a way to reliably detect S3 dispatch?
#     length(make_dispatch_candidate_sets(df)) > 0
# }

# read_header_from_path <- function(path) {
#     f <- file(path)
#     l <- readLines(f)
#     l <- strsplit(l, ",")[[1]]
#     close(f)

#     # Return
#     l 
# }

# remove_dispatch_for_all_funs <- function(df, lof) {
#     # Scary for loop...
#     for (n in lof) {
#         df <- remove_dispatch_for_fun(df, n)
#     }

#     df # return
# }

# These aren't all dispatch, but the next stage benefits from this paring down.
# Some information we compute here isn't strictly necessary right now, but it might
# be helpful if removing dispatch still proves to be inefficient.
#
# TODO: Worth double-checking if this can be smarter.
# make_dispatch_candidate_sets <- function(df) {
#     unames <- df %>% select(fun_name) %>% unique %>% unlist %>% unname
#     potential_dispatch_names <- unames[grepl("\\.", unames)]

#     # So: it's gonna be the a.b.cs s.t. \exists a.b somewhere
#     # base_fun_names <- setdiff(unames, potential_dispatch_names)

#     # Grab all but the last. a.b from a.b.c, for example.
#     base_fun_names <- map(unames, function(un) {
#         split <- strsplit(un, split="\\.")[[1]]
#         if (length(split) == 1) {
#             # no point considering -- if it could be a base_fun_name, it'll
#             # show up as base_fun_name.<dispatch on...>
#             ""
#         } else {
#             paste(split[1:length(split)-1], collapse=".")
#         }
#     })

#     base_fun_names <- base_fun_names[base_fun_names != ""] %>% unique

#     intersect(base_fun_names, unames)
# }

# make_S4_dispatch_candidate_sets <- function(df) {
#     # Assuming there's S4 dispatch happening.
#     # Get rows that have S4 stuff.
#     key_dispatch_rows <- df %>% get_S4_rows %>% 

#     # TODO: check that this always works.
#     # Filter out rows for base and methods package calls.
#     filter(!(package == "base" | package == "methods"))

#     # Now, we want to remove the double counting.
#     # We can do that by looking at the function IDs.
#     # Need to find the base ID of the function itself.
#     uIDs <- key_dispatch_rows %>% select(fun_id) %>% unique %>% unlist %>% unname

#     # Ok. One of the TOTAL COUNTS will be half of all total counts.
#     # Get per-uID counts.
#     counts_per_uID <- key_dispatch_rows %>% group_by(fun_id) %>% summarize(count = sum(count))

#     tot_count <- counts_per_uID %>% select(count) %>% sum

#     base_fun_id <- counts_per_uID %>% filter(count == tot_count / 2) %>% select(fun_id) %>% unlist %>% unname

#     # We can get rid of the rows with the base fun ID, because they appear in the data 
#     # through their dispatch cases. 
#     # If we are still interested in consolidating the data, maybe we can create "groups" of
#     # functions that share a base method.
#     # Return:
#     df %>% filter(! fun_id == base_fun_id)
# }

# # TODO: S4 Dispatch
# old_deal_with_dispatch <- function(df, base_fun_name, base_package="base") {
#     u_fun_names <- df %>% select(fun_name) %>% unique %>% unlist %>% unname

#     # for returning
#     df_r <- df

#     # ok, there are TWO cases we might want to consider here... are there?

#     # 1. entirely different function names e.g. +.CLASS
#     for (n in setdiff(u_fun_names, base_fun_name)) {
#         df_r <- remove_dispatch(df_r, n, base_fun_name)
#     }

#     # 2. the same function, redefined in another package
#     package_names <- df %>% select(package) %>% unique %>% unlist %>% unname

#     for (p in setdiff(package_names, base_package)) {
#         df_r <- remove_fun_redefs(df_r, p, base_package, base_fun_name)
#     }

#     # return
#     df_r
# }

# # easy processor for base package functions
# results_for_base_fun <- function(base_fun_name, dir="~/results_1000") {
#     lof <- list.files(dir, full.names=T)

#     # debug
#     lof <- head(lof, 2)

#     Reduce(function(x, y) {
#         combo_df <- bind_rows(x, filter(get_all_base_in_dir(y), fun_name==base_fun_name))
#         combo_df[,colSums(is.na(combo_df))<nrow(combo_df)]
#     }, lof, data.frame())

# }

# # extract the dimensions from a given type
# extract_dim_from_type_old <- function(t) {
#     r <- sub("\\].*", "", sub(".*\\[", "", remove_tags_from_type(t)))
#     if (r == t)
#         NA
#     else
#         r
# }

# extract_dims_from_type_list <- function(lot) {
#     r <- map(lot, function(t) {
#         t %>% extract_dim_from_type_old
#     }) %>% unname %>% unlist

#     r[!is.na(r)]
# }

# remove_full_NA_cols <- function(df) {
#     df[,colSums(is.na(df))<nrow(df)]
# } 

# TODO: take advantage of flows to track more than just type?
# e.g. if we can link arguments with returns

# make_names_for_dims <- function(dims) {
#     # TODO finish alphabet
#     alphabet <- c("a", "b", "c", "d", "e", "f", "g", "h", "i")

#     alphabet[1:length(dims)]
# }

# replace_dim <- function(t, replacements) {
#     t_s <- strsplit(t, split="@", fixed=T)[[1]]

#     type <- substr(t_s[1], 1, nchar(t_s[1])-1)
#     type_s <- strsplit(type, split="[", fixed=T)[[1]]
#     the_dim <- strsplit(type_s[2], split="-", fixed=T)[[1]]

#     if (length(the_dim) == 1) { # if vector,
#         type <- paste(type_s[1], replacements[[the_dim]], sep="[")
#     } else { # else if matrix
#         the_dim <- sapply(the_dim, function(d) {
#             if (d %in% names(replacements))
#                 replacements[[d]]
#             else
#                 d
#         })
#         type <- paste(type_s[1], paste(the_dim, collapse="-"), sep="[")
#     }
#     t_s[1] <- paste(type, "]", sep="")

#     paste(t_s, collapse="@")
# }

# has_dim <- function(t) {
#     length(strsplit(t, split="[", fixed=T)[[1]]) > 1
# }

# TODO: we want to know when we should incorporate the class in the type


# TODO incorporate rudimentary subtyping
# TODO not resilient to rows with different lengths
# crunch_trace <- function(t) {
#     # 1. collect all types in trace
#     #    works best on tagless types
#     tnames <- names(t)
#     tnames <- tnames[grep("_t", tnames)]

#     types <- as.list(t[tnames])
#     types <- types[!is.na(types)]

#     # save tags
#     split <- lapply(types, function(x) strsplit(x, "@", fixed=T)[[1]])

#     types <- lapply(split, function(x) x[1])

#     # transform PRIM[i] into PRIM
#     scalar_inds <- types %in% paste(PRIM_TYPES, "[1]", sep="")
#     types[scalar_inds] <- substr(types[scalar_inds], 0, nchar(types[scalar_inds]) - 3) 

#     #
#     #   TODO -- do lists and data.frames, not just primitives
#     #        -- do matrix dim logic for vectors also
#     #

#     # this crunches the return ~ argument length dependency
#     # if (strsplit(types[[1]], "[", fixed=T)[[1]][1] %in% PRIM_TYPES) {
#     if (extract_dims_from_type_list(types) %>% length >= 1) {
#         # new logic based primarily on the dimensions
#         # we know that the return type is a primitive vec of some sort
#         # all_dims <- lapply(types, extract_dim_from_type)
#         all_dims <- extract_dims_from_type_list(types)

#         split_dims <- map(all_dims, function(s) strsplit(s, split="-")[[1]])
#         #if (split_dims %>% map(length) %>% reduce(`+`) != length(split_dims)) {
#             # there's a matrix
#             udims <- split_dims %>% reduce(c) %>% unique
#             var_name_replacements <- make_names_for_dims(udims)
#             names(var_name_replacements) <- udims

#             types[sapply(types, has_dim)] <- sapply(types[sapply(types, has_dim)], function(t) {
#                 replace_dim(t, var_name_replacements)
#             })
#         #} else {
#         #    # no matrices, simple case
#         #    if (all_dims[[1]] %in% all_dims[2:length(all_dims)]) {
#         #        types[all_dims == all_dims[[1]]] <- sapply(types[all_dims == all_dims[[1]]], function(t) {
#         #            # TODO: make work for matrices by checking the dimension
#         #            #       needs more parsing...
#         #            paste(strsplit(t, "[", fixed=T)[[1]][1], "[n]", sep="")
#         #        })
#         #    }
#         #}
#     }

#     types <- mapply(function(t, s) {
#         s[1] <- t
#         paste(s, collapse="@")
#     }, types, split)

#     types
# }

# # crunch a data frame of traces down to a data frame of signatures
# # DOES NOT PLAY WELL WITH ROWS OF DIFFERENT LENGTH
# crunch_sigs <- function(df_t) {
#     # 1. transform the types?
#     # TODO: will turn dimensions into n?
#     # TODO: other whole sig transformations...
#     type_names <- names(df_t)
#     type_names <- type_names[grep("_t", type_names, fixed=T)]

#     # NOTE t for transpose
#     df_t[type_names] <- t(apply(df_t[type_names], 1, function(r) {
#         crunch_trace(r)
#     }))

#     df_t

# }

######################################################
#
# Functions for transforming types.
#

# make_class_type_trace <- function(t) {
#     # do it for one trace
#     classes <- t[grepl("_c", names(t), fixed=T)]
#     classes <- substr(classes, 2, nchar(classes) - 1)
#     type_inds <- grep("_t", names(t), fixed=T)

#     t[type_inds[classes != ""]] <- classes[classes != ""]

#     # t[grepl("_t", names(t)[nchar(t[grepl("_c", names(t), fixed=T)]) > 2], fixed=T)] <- substr(t[grepl("_c", names(t)[nchar(t[grepl("_c", names(t), fixed=T)]) > 2], fixed=T)], 2, nchar(t[grepl("_c", names(t)[nchar(t[grepl("_c", names(t), fixed=T)]) > 2], fixed=T)]) - 1)

#     t
# }

# make_class_type_whole_df <- function(df) {
#     # map(df, make_class_type_trace) %>% as_tibble

#     df_r <- t(apply(df, 1, make_class_type_trace))

#     df_r <- as_tibble(df_r)

#     # the approach above turns numeric columns into character columns, fix that
#     df_r$count <- as.double(df_r$count)
#     if ("trace_hash" %in% names(df_r))
#         df_r$trace_hash <- as.double(df_r$trace_hash)

#     df_r
# }

# also remove environment names! TODO
# remove_tags_from_type <- function(ty, except_function_tags=T) {
#     if (!NAMES_REMOVED) # TODO this wont work actually
#         the_split <- strsplit(strsplit(ty, "@", fixed=T)[[1]][1], split="{", fixed=T)[[1]]
#     else 
#         the_split <- strsplit(ty, "@", fixed=T)[[1]]

#     if (except_function_tags && the_split[1] %in% c("closure", "builtin", "special")) {
#         paste(the_split[1], the_split[2], sep="@")
#     } else {
#         the_split[1]
#     }
# }

# remove_all_tags_from_df <- function(df, except_function_tags=T) {
#     df[,names(df)[grep("_t", names(df))]] <- map(df[,names(df)[grep("_t", names(df))]], function(lot) {
#         map(lot, function(t) remove_tags_from_type(t, except_function_tags)) %>% unlist
#     })

#     df
# }

# remove_dimensions_from_type <- function(ty) {
#     strsplit(ty, "[", fixed=T)[[1]][1]
# }

# transform_dimension_to_scalar_vector <- function(ty) {
#     ty_split_tags <- strsplit(ty, "@", fixed=T)[[1]]

#     tagless_ty <- ty_split_tags[1]
#     split_ty <- strsplit(substr(tagless_ty, 1, nchar(tagless_ty)-1), split="\\[")[[1]]

#     # is it a matrix?
#     if (length(strsplit(split_ty[2], split=",")[[1]]) == 2) {
#         # it is a matrix, forget about it?
#         ty_split_tags[1] <- paste("m", split_ty[1], sep="/")
#         return(paste(ty_split_tags, collapse="@"))
#     }

#     if (as.integer(split_ty[2]) == 1) {
#         ty_split_tags[1] <- paste("s", split_ty[1], sep="/")
#         return(paste(ty_split_tags, collapse="@"))
#     }
#     else if (!is.na(as.integer(split_ty[2]))) {
#         ty_split_tags[1] <- paste("v", split_ty[1], sep="/")
#         return(paste(ty_split_tags, collapse="@"))
#     }
#     else
#         return(ty)
# }

# lot : list of tagless types
# consolidate_dimension_types <- function(lot) {
#     pred <- function(t) {
#         length(strsplit(t, "[", fixed=T)[[1]]) > 1
#     }

#     # dim_types will have the types which have dimensions
#     dim_types <- lot[sapply(lot, pred)]

#     # we should now consolidate them if we can

# }

#
#
#
######################################################

# ???
# TODO: unhardcode this
# extract_and_save_base_fun <- function(df, a_fun_name) {
#     file_part <- a_fun_name
#     if (grepl("/", a_fun_name, fixed=T)) {
#         file_part <- str_replace(a_fun_name, "/", "slash")
#     }

#     df %>% filter(fun_name == a_fun_name) %>% write_csv(paste("~/base_only_1000/base_", file_part, ".csv", sep=""))

#     invisible(TRUE)
# }

######################################################
#
# Functions for consolidating/minimizing data
#

# remove environment {...} stuff
# remove_env_names_from_line <- function(l) {
#     # gsub("environment\\{((?!\\}).)*?\\}", "environment", l, perl=T)
#     gsub("environment\\{[^\\}]*\\}", "environment", l, perl=T)
# }

# clean (version >= 2.2) and \n stuff from package list
# for ECOOP experiments
# clean_imports_from_avail_pkgs <- function(s) {
#     gsub("\n", "", gsub("\\([^\\)]*\\)", "", s, perl=T))
# }

# split_imports_list <- function(l) {
#     map(l, clean_imports_from_avail_pkgs) %>%
#     map(function(s) {strsplit(s, ",")[[1]] %>% map(str_trim) %>% unlist})
# }

generate_header_for_num_args <- function(n) {
    dyn_col_names <- c(
        c("arg_t_r", "arg_c_r", "arg_a_r"), 
        mapply( function(x, y) paste(x, y, sep=""), 
        rep(c("arg_t", "arg_c", "arg_a"), n), 
        rep(seq_len(n), each=3) - 1) %>% unname)

    c(PREAMBLE, dyn_col_names)
}

generate_header_types_for_num_args <- function(n) {
    dyn_col_types <- rep(c("c", "c", "c"), n+1)

    paste0(c(PREAMBLE_TYPES, dyn_col_types), collapse="")
}

# 476 in big df now
# generate_col_names_for_count <- function(ct) {
#     num_to_gen <- (ct - NUM_PREAMBLE) / 3 - 1 # - 1 for return

#     dyn_col_names <- c(
#         c("arg_t_r", "arg_c_r", "arg_a_r"), 
#         mapply( function(x, y) paste(x, y, sep=""), 
#         rep(c("arg_t", "arg_c", "arg_a"), num_to_gen), 
#         rep(seq_len(num_to_gen), each=3) - 1) %>% unname)

#     c(PREAMBLE, dyn_col_names)
# }

# make_and_minimize_base_df <- function(path_to_csv, col_names) {
#     df <- read_csv(path_to_csv, col_names = col_names)

#     df %>%
#     group_by_at(setdiff(col_names, "count")) %>%
#     summarize(count=sum(count))
# }

# big_csv_get_col_names <- function(path) {
#     # get the lines
#     f <- file(path)
#     lines <- readLines(f)
#     close(f)

#     # find biggest line
#     max <- max(sapply(lines, function(l) { length(strsplit(l, split=",")[[1]]) }))

#     max
# }

# big_csv_get_all_row_lengths <- function(path) {
#     f <- file(path)
#     lines <- readLines(f)
#     close(f)

#     map(lines, function(l) { length(strsplit(l, split=",")[[1]]) })
# }

# bind_many_dfs <- function(dfs) {

#     while(length(dfs) > 1) {

#         dfs1 <- dfs[seq(1, length(dfs), 2)]
#         dfs2 <- dfs[seq(2, length(dfs), 2)]

#         # PAD
#         if (length(dfs1) > length(dfs2)) {
#             dfs2[[length(dfs1)]] <- data.frame()
#         } else if (length(dfs2) > length(dfs1)) {
#             dfs1[[length(dfs2)]] <- data.frame()
#         }
        
#         dfs <- mapply(function(df1, df2) {
#             b <- bind_rows(df1, df2)

#             b %>%
#             group_by_at(setdiff(names(b), "count")) %>%
#             summarize(count=sum(count)) %>%
#             ungroup
#         }, dfs1, dfs2, SIMPLIFY=F)

#     }

#     # postprocess
#     dfs[[1]]
# }

# bind_many_dfs_bad <- function(dfs) {
#     tot_rows <- Reduce(`+`, sapply(dfs, nrow))
#     max_cols <- max(sapply(dfs, ncol))
#     names_of_max_col <- names(dfs[sapply(dfs, ncol) == max_cols][[1]]) # [[1]] gets first of many
    
#     # make a df of the appropriate size
#     df <- setNames(data.frame(matrix(ncol=max_cols, nrow=tot_rows)), names_of_max_col)

#     # very inefficient
#     r <- 1
#     for (i in 1:length(dfs)) {
#         this_df <- dfs[[i]]
#         for (j in 1:nrow(dfs[[i]])) {
#             df[r,] <- this_df[j,]
#             cat("working...", r)
#             r <- r + 1
#         }
#     }

#     as_tibble(df) # return
# }

# Jan 7 -- 6015 is how many columns there should be atm?
# generate_col_types_for_count <- function(num_args, tot_cols) {
#     # "-" is skip
#     pre_compact <- paste(PREAMBLE_TYPES, collapse="")
#     arg_compact <- paste(rep("c", 3*num_args), collapse="")
#     how_many_to_ignore <- tot_cols - NUM_PREAMBLE - 3*num_args
#     ign_compact <- paste(rep("-", how_many_to_ignore), collapse="")

#     paste(pre_compact, arg_compact, ign_compact, sep="")
# }

# collapse_package_sources_to_count <- function(df) {
#     df$num_packages_called_from <- rep(1, nrow(df))

#     pre <- c(setdiff(PREAMBLE, "package_being_analyzed"), "num_packages_called_from")
#     post <- setdiff(names(df), PREAMBLE_WITH_TYPES_HASH)

#     df %>% group_by(trace_hash) %>% mutate(num_packages_called_from = sum(num_packages_called_from)) %>%
#       select(c(pre, post))
# }

# is_primitive_vector <- function(t) {
#     map(strsplit(t, split="[", fixed=T), function(b) {
#         if (length(b) <= 1)
#             return(FALSE)
#         else
#             b[1] %in% PRIM_TYPES
#     }) %>% unlist
# }

top_level_split <- function(t, split, just_one=TRUE, init_num_brackets = 0, init_num_chevrons = 0) {
    if (!just_one) {
        return(map(t, function(ti) top_level_split(ti, split, just_one=TRUE)))
    }
    
    if (is.na(t))
        return(NA)

    num_brackets <- init_num_brackets
    num_chevrons <- init_num_chevrons

    parsing_name <- FALSE

    inds <- c(0)
    for (i in 1:nchar(t)) {
        the_char <- substr(t, i, i)

        if (the_char == "`")
            parsing_name <- !parsing_name
        
        if(!parsing_name) {
            if (the_char == "[") {
                num_brackets <- num_brackets + 1
            } else if (the_char == "<") {
                num_chevrons <- num_chevrons + 1
            } else if (the_char == "]") {
                num_brackets <- num_brackets - 1
            } else if (the_char == ">") {
                num_chevrons <- num_chevrons - 1
            } else if (the_char == split && num_brackets == 0 && num_chevrons == 0) {
                inds <- c(inds, i)
            }
        }
    }

    inds <- c(inds, nchar(t))

    # inds contains 0, and the indices of split
    if (length(inds) == 1) {
        return(list()) # no tags
    }
    r <- c()
    for (i in 1:(length(inds) - 1)) {
        if (i == length(inds) - 1) 
            r <- c(r, substr(t, inds[i] + 1, inds[i+1]))
        else
            r <- c(r, substr(t, inds[i] + 1, inds[i+1] - 1))
    }

    r
}

# has_na_free_tag <- function(t) {
#     ts <- map(t, function(e) top_level_split(e, "@"))
    
#     map(ts, function(e) {
#         length(e[e == "NA-free"]) == 1
#     }) %>% unlist
#     # length(ts[ts == "NA-free"])
# }

#
#
# Quick Transformation Functions
# Should just take the data.frame.
#

# "quick" -- this function is slow
# quick_get_type_and_class_counts <- function(df) {
#     # Grab all type columns.
#     df %>% select(package_being_analyzed, starts_with("arg_t"), starts_with("arg_c"), count) %>% 
#         # Pivot longer, making a row for every column, copying count and package_being_analyzed. 
#         pivot_longer(-c(package_being_analyzed, count), names_to="pos", values_to="type") %>% 
#         # We actually don't care about pos, and don't want NAs.
#         select(-pos) %>% filter(!is.na(type)) %>% 
#         # Group and summarize.
#         group_by(package_being_analyzed, type) %>% 
#         summarize(count = sum(count))
#         # End product is a data.frame with a column for package_being_analyzed, count, and the type.
# }

# df %>% select(package_being_analyzed, starts_with("arg_c"), count) %>% 
#         # Pivot longer, making a row for every column, copying count and package_being_analyzed. 
#         pivot_longer(-c(package_being_analyzed, count), names_to="pos", values_to="class")

# "quick" -- this function is slow
quick_get_type_counts <- function(df) {
    # Grab all type columns.
    df %>% select(package_being_analyzed, starts_with("arg_t"), count) %>% 
        # Pivot longer, making a row for every column, copying count and package_being_analyzed. 
        pivot_longer(-c(package_being_analyzed, count), names_to="pos", values_to="type") %>% 
        # We actually don't care about pos, and don't want NAs.
        select(-pos) %>% filter(!is.na(type)) %>% 
        # Group and summarize.
        group_by(package_being_analyzed, type) %>% 
        summarize(count = sum(count))
        # End product is a data.frame with a column for package_being_analyzed, count, and the type.
}

quick_get_type_counts_with_num_funs <- function(df) {
    # Grab all type columns.
    df %>% select(package_being_analyzed, starts_with("arg_t"), count) %>% 
        # Add a column for the number of functions it appeared in.
        mutate(num_funs = 1) %>%
        # Pivot longer, making a row for every column, copying count and package_being_analyzed. 
        pivot_longer(-c(package_being_analyzed, count, num_funs), names_to="pos", values_to="type") %>% 
        # We actually don't care about pos, and don't want NAs.
        select(-pos) %>% filter(!is.na(type)) %>% 
        # Group and summarize.
        group_by(package_being_analyzed, type) %>% 
        summarize(count = sum(count), num_funs = sum(num_funs))
        # End product is a data.frame with a column for package_being_analyzed, count, and the type.
}

quick_get_type_counts_with_row_nums <- function(df) {
    # Grab all type columns.
    df %>% select(package_being_analyzed, starts_with("arg_t"), count) %>% 
        # Add a column for the number of functions it appeared in.
        mutate(from_row = row.names(df)) %>%
        # Pivot longer, making a row for every column, copying count and package_being_analyzed. 
        pivot_longer(-c(package_being_analyzed, count, from_row), names_to="pos", values_to="type") %>% 
        # We actually don't care about pos, and don't want NAs.
        select(-pos) %>% filter(!is.na(type)) %>% 
        # Group and summarize.
        group_by(package_being_analyzed, type) %>% 
        summarize(count = sum(count), from_row = paste(from_row, collapse=", "))
        # End product is a data.frame with a column for package_being_analyzed, count, and the type.
}

quick_get_type_counts_with_row_nums_package <- function(df) {
    # Grab all type columns.
    df %>% select(package, starts_with("arg_t"), count) %>% 
        # Add a column for the number of functions it appeared in.
        mutate(from_row = row.names(df)) %>%
        # Pivot longer, making a row for every column, copying count and package. 
        pivot_longer(-c(package, count, from_row), names_to="pos", values_to="type") %>% 
        # We actually don't care about pos, and don't want NAs.
        select(-pos) %>% filter(!is.na(type)) %>% 
        # Group and summarize.
        group_by(package, type) %>% 
        summarize(count = sum(count), from_row = paste(from_row, collapse=", "))
        # End product is a data.frame with a column for package, count, and the type.
}

# quick_group <- function(df) {

#     rm_me <- c("count")
#     if ("trace_hash" %in% names(df))
#         rm_me <- c(rm_me, "trace_hash")

#     select_me <- setdiff(PREAMBLE, "trace_hash")
#     select_me <- select_me[select_me %in% names(df)]

#     df %>% group_by_at(setdiff(names(df), rm_me)) %>%
#            summarize(count=sum(count)) %>%
#            ungroup %>%
#            # reorder
#            select(select_me, everything())
# }

# Note: performance is lacking. Is there a way to speed this up?
# Note: essentially, this serves to remove dimensions.
# TODO: optimize this
quick_simplify_types <- function(df) {
    all_og_names <- names(df)
    names_og <- names(df)[grepl("arg_t", names(df), fixed=T)]
    names_tmp <- paste(names_og, "_tmp", sep="")

    trim_tmp <- function(x) {
        map(x, function(e) {strsplit(e, split="_tmp", fixed=T)[[1]]}) %>% unlist
    }

    make_type <- function(x) {
        map(x, function(xx) {
            if (is.na(xx))
                return(xx)

            tagless_x <- top_level_split(xx, "@")
            tags <- ""
            tags_as_list <- list()
            if (length(tagless_x) > 1) {
                tags <- paste0("@", paste(tagless_x[2:length(tagless_x)], collapse="@"))
                tags_as_list <- tagless_x[2:length(tagless_x)]
            }

            tagless_x <- tagless_x[1]

            extracted_dim <- strsplit(tagless_x, "[", fixed=T)[[1]]

            if (extracted_dim[[1]] %in% PRIM_TYPES && length(extracted_dim) > 1) {
                # It's a primitive type.
                # There's a dim.
                the_dim <- strsplit(extracted_dim[[2]], split="]", fixed=T)[[1]]

                if (is.na(the_dim))
                    return(xx)

                if (length(strsplit(the_dim, "-", fixed=T)[[1]]) > 1)
                    return(paste(paste(extracted_dim[[1]], "[,]", sep=""), tags, sep=""))

                the_dim <- as.integer(the_dim)

                if (the_dim == 1)
                    return(paste(paste(extracted_dim[[1]], "", sep=""), tags, sep=""))
                else
                    return(paste(paste(extracted_dim[[1]], "[]", sep=""), tags, sep=""))
            } else if (substr(tagless_x, 1, 5) == "data.") {
                # It's a data.frame.
                # No more dims. Keep tags. The names are stored in a tag for data.frames.

                split_up <- split_up_data_frame_names_and_types(xx)
                split_up <- map(split_up, function(e) {
                    top_level_split(e, ":")
                })

                names <- map(split_up, function(e) e[1])
                types <- make_type(map(split_up, function(e) e[2]))
                split_up <- map2(names, types, function(n, t) paste(n, t, sep=":"))

                tags_as_list[substr(tags_as_list, 1, 4) == "cols"] <- paste("cols[", paste(split_up, collapse="~"), "]", sep="")

                return(paste("data.frame", paste(tags_as_list, collapse="@"), sep="@"))
            } else if (substr(tagless_x, 1, 5) == "list<") {
                # It's a list, either a struct or heterogeneous list.
                # Read backwards from the end of tagless_x to find [...].

                # First: recurse, then remove dimensions.
                split_up <- split_up_struct_names_and_types(xx)

                new_inner <- ""
                # If it's a heterogeneous list:
                if (length(split_up) == 1 && split_up == "") {
                    tagless_x <- "list<any>" # dont want dimensions, and there's nothing inside anyway.
                } else {
                    new_inner <- map(split_up, make_type)
                    tagless_x <- paste("list<", paste(new_inner, collapse="~"), ">", sep="")
                }

                return(paste(tagless_x, tags, sep=""))
            } else if (substr(tagless_x, 1, 7) == "struct<") {
                # It's a struct.
                # Read backwards from the end of tagless_x to find [...].

                # First: recurse, then remove dimensions
                split_up <- split_up_struct_names_and_types(xx)
                split_up <- map(split_up, function(e) {
                    top_level_split(e, ":") # This could be better if we accoutn for "`:"
                })

                new_inner <- ""

                names <- map(split_up, function(e) e[1])
                types <- make_type(map(split_up, function(e) e[2]))
                split_up <- map2(names, types, function(n, t) paste(n, t, sep=":"))
                
                new_inner <- paste(split_up, collapse="~")
                tagless_x <- paste("struct<", new_inner, ">", sep="")

                return(paste(tagless_x, tags, sep=""))
            } else if (substr(tagless_x, 1, 6) == "class<") {
                # This is ok. Just put it.
                return(tagless_x)
            }

            # At this point, there's nothing to do. Let's just return what we were passed.
            # Since we want the tags anyway.
            xx
        }) %>% unlist %>% unname
    }

    # First, get all arg_tX columns, and make the types.
    df %>% mutate_at(vars(starts_with("arg_t")), .funs = list(tmp = ~ make_type(.))) %>%
           # Turn the new columns (arg_tx_tmp) into arg_tx.
           select(-names_og) %>% rename_all(trim_tmp) %>%
           # Reorder (according to initial order)
           select(all_og_names)
}

# check_eq_type <- function(df, t) {
#     df %>% filter_at(vars(starts_with("arg_t")), any_vars(. == t))
# }

# Filter out internal packages, including the base package and other utility packages.
# If so desired, can specify additional packages to filter.
# 
# List of internal packages:
INTERNAL_PACKAGES <- c("base", "methods", "rlang", "stats", "utils", "data.table", "codetools")
#
quick_filter_out_internal_packages <- function(df, additional_packages = c()) {
    df %>% filter(! package %in% c(INTERNAL_PACKAGES, additional_packages))
}

quick_trim_na_cols <- function(df) {
    not_all_na <- function(x) {!all(is.na(x))}
    df %>% select_if(not_all_na)
}

# quick_remove_tags <- function(df, except_function_tags=T) {
#     remove_all_tags_from_df(df, except_function_tags)
# }

# quick_type_cols_only <- function(df) {
#     col_names_for_types <- names(df)[grepl("_t", names(df), fixed=T)]

#     df %>% select(c("package_being_analyzed", "package", "fun_name", "count", col_names_for_types))
# }

# quick_type_and_class_cols_only <- function(df) {
#     col_names_for_types <- names(df)[grepl("_t|_c", names(df), fixed=F)]

#     df %>% select(c("package_being_analyzed", "package", "fun_name", "count", col_names_for_types))
# }

# quick_crunch <- function(df_m) {
#     df_m %>% quick_trim_na_cols %>% quick_remove_tags %>% crunch_sigs %>% quick_group
# }

# quick_remove_dims <- function(df) {
#     col_names_for_types <- names(df)[grepl("_t", names(df), fixed=T)]

#     df[, col_names_for_types] <- df[, col_names_for_types] %>% map(function(l) map(l, function(t) remove_dimensions_from_type(t)) %>% unlist)

#     df
# }

my_read_csv <- function(path) {
    f <- file(path)
    l <- readLines(f, 1)
    close(f)

    ncol <- length(strsplit(l, ",", fixed=T)[[1]])

    col_types <- paste0("cccccl", paste(rep("c", ncol - 7), collapse=""), "i")

    read_csv(path, col_types = col_types)
}

# TODO: should we reorder? Not necessary, and might be kind of annoying.
# quick_separate_tags_into_own_columns <- function(df) {

#     all_og_names <- names(df)
#     names_og <- all_og_names[grepl("arg_t", all_og_names, fixed=T)]
#     label_names <- paste(names_og, "_tags", sep="")

#     extract_tags <- function(col) {
#         map(col, function(type) {
#         if (is.na(type))
#             return(NA)
        
#         split <- strsplit(type, "@", fixed=T)[[1]]
        
#         if (length(split) == 1)
#             NA
#         else {
#             # TEMPORARY: TO DEAL WITH fn-id.
#             the_split <- split[2:length(split)]
#             the_split <- map(the_split, function(x) {
#                 if (substr(x, 1, 6) == "fn-id;")
#                     substr(x, 7, nchar(x))
#                 else   
#                     x
#             })
#             paste(the_split, collapse=",")

#             # TODO: Delete rest, and uncomment this when ready to roll.
#             # paste(split[2:length(split)], collapse=",")
#         }
#         }) %>% unlist
#     }

#     rename_fun <- function(sz) {
#         map(sz, function(s) {
#             if (grepl("_tags", s, fixed=T)) {
#                 ss <- strsplit(strsplit(s, "arg_t", fixed=T)[[1]][2], "_tags", fixed=T)[[1]]
#                 paste("arg_l", ss, sep="")
#         } else
#             s
#         })
#     }
    
#     df %>% mutate_at(vars(starts_with("arg_t")), .funs = list(tags = ~ extract_tags(.))) %>%
#            # Need to rename, need another "name" for tags. Info? Modifier? Label!
#            rename_at(label_names, rename_fun)
# }

# quick_separate_dims_into_own_columns <- function(df) {

#     all_og_names <- names(df)
#     names_og <- all_og_names[grepl("arg_t", all_og_names, fixed=T)]
#     label_names <- paste(names_og, "_dim", sep="")

#     extract_dims <- function(col) {
#         map(col, function(type) {
#             if (is.na(type))
#                 return(NA)
            
#             split <- strsplit(type, "[", fixed=T)[[1]]
            
#             if (length(split) == 1)
#                 NA
#             else {
#                 # TEMPORARY: TO DEAL WITH fn-id.
#                 the_dim <- split[2]
#                 the_dim <- strsplit(the_dim, "]", fixed=T)[[1]][1]

#                 the_dim
#             }
#         }) %>% unlist
#     }

#     rename_fun <- function(sz) {
#         map(sz, function(s) {
#             if (grepl("_dim", s, fixed=T)) {
#                 ss <- strsplit(strsplit(s, "arg_t", fixed=T)[[1]][2], "_dim", fixed=T)[[1]]
#                 paste("arg_d", ss, sep="")
#         } else
#             s
#         })
#     }
    
#     df %>% mutate_at(vars(starts_with("arg_t")), .funs = list(dim = ~ extract_dims(.))) %>%
#            # Need to rename, need another "name" for tags.
#            rename_at(label_names, rename_fun)
# }

# quick_remove_tags <- function(df) {
#     all_og_names <- names(df)
#     names_og <- names(df)[grepl("arg_t", names(df), fixed=T)]

#     df %>% mutate(vars(starts_with("arg_t")), .funs = list(tmp = ~  str_extract(., "[^@]*"))) %>%
#            # Turn the new columns (arg_tx_tmp) into arg_tx.
#            select(-names_og) %>% rename_all(trim_tmp) %>%
#            # Reorder (according to initial order)
#            select(all_og_names)
# }

# I suppose this can be run in parallel before combining everything
# quick_add_type_digest <- function(df) {
#     # Compute the digest.
#     # First, get names of columns for digest.
#     cols_we_want <- names(df)[grepl("arg", names(df))]
#     digests <- df[, cols_we_want] %>% rowwise %>% do(row = digest(.)) %>% unlist %>% unname

#     # Put it in df.
#     df$types_hash <- digests

#     pre_here <- intersect(PREAMBLE, names(df))

#     # Reorder df and return.
#     df[, c(pre_here, "types_hash", setdiff(names(df), c(pre_here, "types_hash")))]
# }

# + 1 for argument type
quick_trim_df_to_x_args <- function(df, how_many = 20) {
    df[,1:min((NUM_PREAMBLE + 3*(how_many+1)), ncol(df))]
}

# NOTE: this is just for arg_t0, but maybe it gets the point across?
# TODO: make work over all argument positions
# quick_get_vec_structs <- function(df) {
#     df %>% filter(str_detect(arg_t0, "names") & str_detect(arg_t0, "list", negate=T) & str_detect(arg_t0, "data.frame", negate=T))
# }

# quick_get_lists_and_vec_structs <- function(df) {
#     df %>% filter(str_detect(arg_t0, "names") | str_detect(arg_t0, "list<"))
# }

# Predicates
# quick_pred_is_S4_dispatch <- function(df) {
#     df %>% get_S4_rows %>% nrow > 0
# }

# get_S4_rows <- function(df, only_user_def=T) {
#     # Get all type rows.
#     type_row_names <- names(df)[grepl("_t", fixed=T, names(df))]
    
#     # Are there any instances of S4 in a type?
#     if (only_user_def) {
#         # If we only want these, make sure to remove "methods" and "base".
#         df %>% filter_at(type_row_names, any_vars("S4" == .)) %>% filter(! (package == "methods" | package == "base"))
#     } else {
#         df %>% filter_at(type_row_names, any_vars("S4" == .))
#     }
# }

# TODO: maybe we can ... learn these, somehow? These parameters make me a little uncomfortable.
# Also, unclear how useful this is going to be. The logic for doing subtyping and all that is gonna be
# pretty complicated, I think. 
# TS0 <- list(
#     any_threshold = 5,
#     parameterized_threshold = 5
# )

# prep_df_for_function_type_inference <- function(df, fn_id) {
#     # First, filter to get the fn_id we want.
#     df %>% filter(fun_id == fn_id) %>% 
#         # Then, simplify the types. TODO: we could use an alternate simplification function here,
#         # once such a function is implemented.
#         quick_simplify_types %>%
#         # Then, get rid of the things that we don't want (arg_c*, arg_a*, and some preamble).
#         quick_trim_na_cols %>% 
#         select(-matches("arg_a|arg_c"), -package_being_analyzed, -package, -fun_name, -trace_hash, -type_hash, -dispatch) %>%
#         # Group, summarize count, ungroup.
#         group_by_at(vars(-count)) %>% summarize(count=sum(count)) %>% ungroup
# }

# process_S3_use_file_to_list_of_files <- function(path) {
#     f <- file(path)
#     l <- readLines(f)
#     close(f)

#     l <- map(l, function(e) {
#         s <- strsplit(e, " : ")[[1]]

#         if(length(s) != 2) {
#             # Error in output file
#             s[1] <- ""
#             s[2] <- "FALSE"

#             # Output
#             s
#         } else {
#             s_1_l <- strsplit(s[1], "/")[[1]]
#             s[1] <- s_1_l[length(s_1_l)]

#             # Output
#             s
#         }
#     })
    
#     l <- l[map(l, function(e) as.logical(e[2])) %>% unlist %>% unname]
    
#     # Return names only
#     map(l, function(e) e[1]) %>% unlist
# }

# process_S4_use_file_to_list_of_files <- function(path) {
#     # Right now, this is the same as S3, so just do that.
#     process_S3_use_file_to_list_of_files(path)
# }

# read_csv_with_header <- function(file_name, path_to_file, path_to_header) {
#     df <- read_csv(paste(path_to_file, file_name, sep="/"), col_names=read_header_from_path(paste(path_to_header, file_name, sep="/")))
# }

#
#
#
######################################################

###
#
#   Cleaning functions
#

# clean_NA_cols <- function(df) {
#     map(df, function(col) { col %>% map(is.na) %>% unlist %>% sum }) -> na_counts

#     df[na_counts != nrow(df)]
# }

# get_len_of_fst_line <- function(path) {
#     f <- file(path)
#     l <- readLines(f, n=1)
#     close(f)

#     if (length(l) == 0)
#         return(0)

#     strsplit(l, split=",")[[1]] %>% length
# }

###
#
# Generate smaller files.
#
#

trim_csv_to_x_args <- function(s_path, t_path, pname, how_many = 20) {
    df <- read_csv(paste(s_path, "/", pname, ".csv", sep=""), col_names=F)

    write_csv(df %>% quick_trim_df_to_x_args, paste(t_path, "/", pname, ".csv", sep=""), col_names=F)
}

# Maybe useful?
#
#

swap_last_col_with_xth_col <- function(df, x=5) {
    df[, x:ncol(df)] <- df[, c(ncol(df), x:(ncol(df)-1))] 
    
    # return 
    df
}

write_csv_for_reading <- function(df, path) {
    want <- c(c("package", "fun_name", "fun_id", "count"), names(df)[grepl("arg", fixed=T, names(df))])

    df %>% select(want) %>% write_csv(path)
}

#
# For breaking down the data.
#
#
# breakdown_dispatch <- function(df) {    
#     df %>% select(package, dispatch, count) %>% group_by(dispatch) %>% summarize(count = sum(count)) %>% mutate(percentage = count / sum(df %>% select(count)))
# }

# breakdown_dispatch_by_pkg <- function(df) {
#     df_dispatch <- df %>% select(package, dispatch, count) %>% group_by(package, dispatch) %>% summarize(count=sum(count))

#     num_tt <- df_dispatch %>% filter(dispatch == "None") %>% nrow

#     num_S3 <- df_dispatch %>% filter(dispatch == "S3") %>% nrow

#     num_S4 <- df_dispatch %>% filter(dispatch == "S4") %>% nrow

#     # Return
#     data.frame( Dispatch = c("Total", "S3", "S4"), 
#                 NumberOfPackages = c(num_tt, num_S3, num_S4), 
#                 Percentage = 100 * c(1, num_S3/num_tt, num_S4/num_tt)) %>% as_tibble
# }

# breakdown_S3_dispatch_by_class <- function(df) {
#     df %>% filter(dispatch == "S3") %>% select(count, arg_c0) %>% group_by(arg_c0) %>% summarize(count=sum(count)) %>% arrange(-count)
# }

# # NOTE TO SELF: mutate shouldnt remove the columns. Do that.
# get_multi_classes <- function(df) {
#     class_cols <- names(df)[grepl("_c", names(df), fixed=T)]
#     class_counts_cols <- paste(class_cols, "_c", sep="")

#     # This will convert the classes to counts. We could also add entirely new columns for this, consider that option.
#     # df_class_counts <- df %>% mutate_at(vars(class_cols), function(e) unlist(map(e, function(e) if (is.na(e)) 0 else length(strsplit(substr(e, 2, nchar(e)-1), "-")[[1]]))))
#     df_class_counts <- df %>% mutate_at(vars(class_cols), .funs = list(c = ~ unlist(map(., function(e) if (is.na(e)) 0 else length(strsplit(substr(e, 2, nchar(e)-1), "-")[[1]])))))

#     # Now, isolate those cases where an argument to a function has multiple classes, and we can manually look at those.
#     # Removing base because there's just so much base.
#     # class_cols[2:length(class_cols)] is class_cols sans the return class
#     df_class_counts %>% filter_at(class_counts_cols[2:length(class_counts_cols)], any_vars(. > 1)) %>% filter(package != "base")
# }

# get_type_class_match <- function(df) {
#     # Transform df into a data frame with type, class, and frequency columns.
#     # Before that, get the total number of calls to make a percentage.
#     tot_calls <- df %>% select(count) %>% sum 

#     # 1. Grab all the type, class, and count columns.
#     df %>% select(starts_with("arg_t"), starts_with("arg_c"), count) %>% 
#         # 2. Stack the data into three columns: type, class, and frequencies
#         pivot_longer(-count, names_to = c(".value", "pos"), values_drop_na=T, names_pattern = "arg_(.)(.)") %>%
#         # 3. Rename the columns, and remove the useless column introduced by pivot_longer.
#         rename(type = t, class = c, frequency = count) %>% select(type, class, frequency) %>%
#         # 4. Group by type and class, collapse frequencies.
#         group_by(type, class) %>% summarize(frequency = sum(frequency)) %>%
#         # 5. Ungroup.
#         ungroup %>% arrange(-frequency) %>%
#         # 6. Add percentage of total calls column.
#         mutate(percentage = 100 * frequency / tot_calls)
# }

# get_class_attr_match <- function(df) {
#     # Transform df into a data frame with type, class, and frequency columns.
#     # Before that, get the total number of calls to make a percentage.
#     tot_calls <- df %>% select(count) %>% sum 

#     # 1. Grab all the type, class, and count columns.
#     df %>% select(starts_with("arg_c"), starts_with("arg_a"), count) %>% 
#         # 2. Stack the data into three columns: type, class, and frequencies
#         pivot_longer(-count, names_to = c(".value", "pos"), values_drop_na=T, names_pattern = "arg_(.)(.)") %>%
#         # 3. Rename the columns, and remove the useless column introduced by pivot_longer.
#         rename(class = c, attributes = a, frequency = count) %>% select(class, attributes, frequency) %>%
#         # 4. Group by type and class, collapse frequencies.
#         group_by(class, attributes) %>% summarize(frequency = sum(frequency)) %>%
#         # 5. Ungroup.
#         ungroup %>% arrange(-frequency) %>%
#         # 6. Add percentage of total calls column.
#         mutate(percentage = 100 * frequency / tot_calls)
# }

# extract_names_in_type_class_df <- function(df) {
#     # df should have a type column
#     # make sure to only call this on such a df where the types have already been filtered to have names.
#     # Stupid wrapper, as str_match returns a (fucking) matrix for some reason.
#     wrap_str_match_for_1_match <- function(string, pattern) { 
#         # 8: @names[ has 7 characters that we want to skip
#         str_match(string, pattern) %>% as.vector %>% map(function(s) substr(s, 8, nchar(s)-1)) %>% unlist
#     }

#     df %>% mutate(enwl = wrap_str_match_for_1_match(type, "@names\\[.*\\]")) %>% select(-type) %>% rename(names = enwl) %>%
#            select(names, class, frequency, percentage)
# }

# split_up_struct_names_and_types_split_again <- function(lonat) {
#     names <- c()
#     types <- c()

#     for (i in 1:length(lonat)) {
#         the_str <- strsplit(lonat[[i]], ":", fixed=T)[[1]]
#         # In case we're dealing with a tuple:
#         if (length(the_str) == 1) {
#             the_type <- the_str[1]
#             the_name <- ""
#         } else {
#             the_name <- the_str[1]
#             the_type <- paste0(the_str[2:length(the_str)])
#         }
#         names <- c(names, the_name)
#         types <- c(types, the_type)
#     }

#     if (length(names[names == ""] == length(names)))
#         types
#     else {
#         names(types) <- names
#         types
#     }
    
# }

# For splitting structs up.
split_up_struct_names_and_types <- function(t) {
    num_l <- 0
    num_b <- 0

    parsing_name <- FALSE

    indices <- c()

    for (i in 1:nchar(t)) {
        this_char <- substr(t, i, i)

        if (this_char == "`")
            parsing_name <- !parsing_name

        if (!parsing_name) {
            if (num_l == 0 && this_char == "<") {
                indices <- c(indices, i) 
            }

            if (this_char == "<") {
                num_l <- num_l + 1
            } else if (this_char == ">") {
                num_l <- num_l - 1 # matching

                if (num_l == 0) {
                    # here, we are done
                    indices <- c(indices, i)
                    break
                }
            } else if (num_l == 1 && num_b == 0 && this_char == "~") {
                # here, we are at the top level
                indices <- c(indices, i) 
            } else if (this_char == "[") {
                num_b <- num_b + 1
            } else if (this_char == "]") {
                num_b <- num_b - 1
            }
        }
    }

    # take indices and make lists of name:type pairs
    r <- c()
    for (i in 1:(length(indices)-1)) {
        r <- c(r, substr(t, indices[i] + 1, indices[i+1] - 1))
    }

    # return    
    r
}

# Use this to split up ((lists)), [[tuples]], and {{structs}}. 
# Post simpl!
# split_up_names_and_types_brackets <- function(t) {
#     # Form: (( n:t, ... ))
#     # Idea: trim leading and trailing 2 characters, split rest up, name-aware

#     # For dealing with `names`
#     parsing_name <- FALSE

#     # Trim leading and trailing brackets.
#     t <- substr(t, 3, nchar(t) - 2)

#     # How many levels (outside of the first 2) ((, [[, {{ are we in?
#     nest_level <- 0
#     last_char <- ""

#     colon_indices <- c()
#     comma_indices <- c(1)

#     for (i in 1:nchar(t)) {
#         this_char <- substr(t, i, i)

#         if (this_char == "`")
#             parsing_name <- !parsing_name

#         if (!parsing_name) {
#             if (this_char == "[" && last_char == "[") {
#                 nest_level <- nest_level + 1
#             } else if (this_char == "]" && last_char == "]") {
#                 nest_level <- nest_level - 1
#             } else if (this_char == "{" && last_char == "{") {
#                 nest_level <- nest_level + 1
#             } else if (this_char == "}" && last_char == "}") {
#                 nest_level <- nest_level - 1
#             } else if (this_char == "(" && last_char == "(") {
#                 nest_level <- nest_level + 1
#             } else if (this_char == ")" && last_char == ")") {
#                 nest_level <- nest_level - 1
#             } else if (nest_level == 0 && this_char == ":") {
#                 # here, we are at the top level
#                 colon_indices <- c(colon_indices, i) 
#             } else if (nest_level == 0 && this_char == ",") {
#                 comma_indices <- c(comma_indices, i)
#             }
#         }
#         last_char <- this_char
#     }

#     comma_indices <- c(comma_indices, nchar(t) + 1)

#     # take indices and make lists of name:type pairs
#     # deal with one edge case:
#     if (is.null(comma_indices)) {
#         name <- substr(t, 1, colon_indices[1] - 1)
#         type <- substr(t, colon_indices[1] + 1, nchar(t))

#         names(type) <- c(name)
#         type

#     # deal with case where its a list with no names
#     } else if (is.null(colon_indices)) {
#         types <- c()

#         # hack for dealing with fact that we need to, most of the time, account for ", " separator.
#         comma_indices[1] <- -1
#         for (i in 1:(length(comma_indices)-1)) {
#             # + 2 for the fact that the sep is ", "
#             types <- c(types, substr(t, comma_indices[i] + 2, comma_indices[i+1] - 1))
#         }

#         types
#     } else {
#         names <- c()
#         types <- c()
#         for (i in 1:(length(colon_indices))) {
#             names <- c(names, substr(t, comma_indices[i], colon_indices[i] - 1))
#             types <- c(types, substr(t, colon_indices[i] + 1, comma_indices[i+1] - 1))
#         }

#         names(types) <- names
#         types
#     }
# }

split_up_names_and_types <- function(t) {

    # For dealing with `names`
    parsing_name <- FALSE

    # Trim leading and trailing brackets.
    # t <- substr(t, 3, nchar(t) - 2)
    # Trim leading and trailing stuff.
    if (substr(t, 1, 2) == "st") {
        t <- substr(t, 8, nchar(t) - 1)
    } else if (substr(t, 1, 2) == "li") {
        t <- substr(t, 6, nchar(t) - 1)
    } else if (substr(t, 1, 2) == "tu") {
        t <- substr(t, 7, nchar(t) - 1)
    }

    # How many levels (outside of the first 2) ((, [[, {{ are we in?
    nest_level <- 0
    last_char <- ""

    colon_indices <- c()
    comma_indices <- c(1)

    for (i in 1:nchar(t)) {
        this_char <- substr(t, i, i)

        if (this_char == "`")
            parsing_name <- !parsing_name

        if (!parsing_name) {
            if (this_char == "s" && substr(t, i, i+5) == "struct") {
                nest_level <- nest_level + 1
            } else if (this_char == ">" && last_char != "=") {
                nest_level <- nest_level - 1
            } else if (this_char == "l" && substr(t, i, i+3) == "list") {
                nest_level <- nest_level + 1
            } else if (this_char == "t" && substr(t, i, i+4) == "tuple") {
                nest_level <- nest_level + 1
            } else if (nest_level == 0 && this_char == ":") {
                # here, we are at the top level
                colon_indices <- c(colon_indices, i) 
            } else if (nest_level == 0 && this_char == ",") {
                comma_indices <- c(comma_indices, i)
            }
        }
        last_char <- this_char
    }

    comma_indices <- c(comma_indices, nchar(t) + 1)

    # take indices and make lists of name:type pairs
    # deal with one edge case:
    if (is.null(comma_indices)) {
        name <- substr(t, 1, colon_indices[1] - 1)
        type <- substr(t, colon_indices[1] + 1, nchar(t))

        names(type) <- c(name)
        type

    # deal with case where its a list with no names
    } else if (is.null(colon_indices)) {
        types <- c()

        # hack for dealing with fact that we need to, most of the time, account for ", " separator.
        comma_indices[1] <- -1
        for (i in 1:(length(comma_indices)-1)) {
            # + 2 for the fact that the sep is ", "
            types <- c(types, substr(t, comma_indices[i] + 2, comma_indices[i+1] - 1))
        }

        types
    } else {
        names <- c()
        types <- c()
        comma_indices[1] <- comma_indices[1] - 2
        for (i in 1:(length(colon_indices))) {
            names <- c(names, substr(t, comma_indices[i] + 2, colon_indices[i] - 1))
            types <- c(types, substr(t, colon_indices[i] + 1, comma_indices[i+1] - 1))
        }

        names(types) <- names
        types
    }
}

# For splitting up data.frame name/types.
split_up_data_frame_names_and_types <- function(t) {
    num_l <- 1
    num_b <- 0

    parsing_name <- FALSE

    # remove the first bits
    check <- strsplit(t, split="@cols[", fixed=T)[[1]]
    if (length(check) == 1) {
        return(list())
    }
    t <- paste(check[2:length(check)], collapse="@cols[")

    # already have the first one in
    indices <- c(0)

    for (i in 1:nchar(t)) {
        this_char <- substr(t, i, i)

        if (this_char == "`")
            parsing_name <- !parsing_name

        if (!parsing_name) {
            if (this_char == "[") {
                num_l <- num_l + 1
            } else if (this_char == "]") {
                num_l <- num_l - 1 # matching

                if (num_l == 0) {
                    # here, we are done
                    indices <- c(indices, i)
                    break
                }
            } else if (num_l == 1 && num_b == 0 && this_char == "~") {
                # here, we are at the top level
                indices <- c(indices, i) 
            } else if (this_char == "<") {
                num_b <- num_b + 1
            } else if (this_char == ">") {
                num_b <- num_b - 1
            }
        }
    }

    # take indices and make lists of name:type pairs
    r <- c()
    for (i in 1:(length(indices)-1)) {
        r <- c(r, substr(t, indices[i] + 1, indices[i+1] - 1))
    }

    # return    
    r
}

# TODO: Test this a bit more.
# extract_names_from_type <- function(t) {
#     if (substr(t, 1, 7) == "struct<") {
#         ts <- split_up_struct_names_and_types(t)

#         # it's gonna be a struct
#         names <- map(ts, function(ntp) {
#             strsplit(ntp, split=":", fixed=T)[[1]][1]
#         }) %>% unlist

#         names[names != "``"]
    
#     } else if (substr(t, 1, 5) == "list<") {
#         # No names in these.
#         list()
#     } else if (substr(t, 1, 10) == "data.frame") {
#         ts <- split_up_data_frame_names_and_types(t)
        
#         names <- map(ts, function(ntp) {
#             strsplit(ntp, split=":", fixed=T)[[1]][1]
#         }) %>% unlist

#         names[names != ""]
#     } else {
#         ts <- strsplit(t, "@names[", fixed=T)[[1]]
#         if (length(ts) > 1) {
#             # there are names
#             ts <- strsplit(ts[2], "]", fixed=T)[[1]][1]

#             # split within names list is by ~
#             # return the split list
#             r <- strsplit(ts, "~", fixed=T)[[1]]
#             r[r != ""]
#         } else {
#             # no names, return empty list
#             list()
#         }
#     }
# }

#
# Another workhorse function.
# This is for some very rough numbers.
# NOTE: use this on a counts data.frame, from quick_get_type_counts
count_how_many_types_match_pred <- function(df, pred) {
    df %>% filter(pred(type)) %>%
        mutate(num_packages = 1) %>%
        group_by(type) %>%
        summarize(count=sum(count), num_packages=sum(num_packages))
}

# get_hetero_list_type <- function(t) {
#     strsplit(strsplit(t, ">", fixed=T)[[1]][1], "<", fixed=T)[[1]][2]
# }

# TODO: you need to be careful with this one, cause some types have [ in them and no dims.
# for now we are assuming that there are dims and they appear before any other [.
#
# TODO: try to make this work over a list of things? embrace the vectorization
# cause right now we have to do:
#  df_m %>% select(arg_t0, count) %>% mutate(dim = unlist(map(type, extract_dim_from_type)), num_names = unlist(map(type, function(x) length(extract_names_from_type(x)))))
# and ... yikes. The performance is also bad.
# extract_dim_from_type <- function(t) {
#     if (substr(t, 1, 5) == "list<") {
#         type_and_tags <- top_level_split(t, "@")
#         type <- type_and_tags[1]

#         # Go back from the end, find the dimension
#         dim_part <- ""
#         for (i in nchar(type):1) {
#             if (substr(type, i, i) == "[") {
#                 dim_part <- substr(type, i, nchar(type))
#                 break
#             }
#         }

#         # Process the dimpart.
#         the_dim <- substr(dim_part, 2, nchar(dim_part) - 1)

#         the_dim %>% as.integer
#     } else if (substr(t, 1, 7) == "struct<") {
#         # Struct also have their length on the end.
#         # NOTE: This is not true post simpling. 
#         type_and_tags <- top_level_split(t, "@")
#         type <- type_and_tags[1]

#         # Do this by counting names.
#         type_split <- top_level_split(substr(type, 8, nchar(type) - 1), "~")

#         length(type_split) # Just return the length of whatever was inside.

#         # If there's a [dim] at the end of the type.
#         # Go back from the end, find the dimension
#         # dim_part <- ""
#         # for (i in nchar(type):1) {
#         #     if (substr(type, i, i) == "[") {
#         #         dim_part <- substr(type, i, nchar(type))
#         #         break
#         #     }
#         # }

#         # Process the dimpart.
#         # the_dim <- substr(dim_part, 2, nchar(dim_part) - 1)

#         # the_dim %>% as.integer
#     } else if (substr(t, 1, 10) == "data.frame") {
#         dim <- strsplit(t, "]", fixed=T)[[1]][1]
#         dim <- strsplit(dim, "[", fixed=T)[[1]][2]
#         dim <- strsplit(dim, "-", fixed=T)[[1]][2] # 2nd is cols

#         as.integer(dim)
#     } else {
#         ts <- strsplit(t, "[", fixed=T)[[1]]
#         if (length(ts) > 1) {
#             # there are dims
#             ts <- strsplit(ts[2], "]", fixed=T)[[1]][1]

#             # TODO: deal with matrix -- unclear what to do about this.
#             ts
#         } else {
#             # no dim, return empty list
#             NA
#         }
#     }
# }

# extract_dim_from_tuple <- function(lot) {
#     map(lot, function(e) {
#         if (substr(e, 5, 6) == "<>")
#             0
#         else
#             length(top_level_split(e, "~", init_num_chevrons=-1))
#     }) %>% unlist

# }

# # This function takes a long time to run.
# from_counts_df_extract_dim_and_names <- function(df) {
#     df %>% mutate(dim = as.character(unlist(map(type, extract_dim_from_type))), num_names = unlist(map(type, function(x) length(extract_names_from_type(x)))))
# }

from_tuples_counts_df_extract_dim <- function(df) {
    df %>% mutate(dim = as.character(unlist(map(type, extract_dim_from_tuple))))
}

#
# Big function for writing the .csv data file out to the appropriate format for Aviral's tastr validator.
#
write_out_type_file <- function(df) {

}

# Takes a type as formatted in our .csv files, and outputs it in the correct format.
# Basically: parse a type and figure out what we want to write out.
# This function might change a lot, depending on the output of the analysis.
# Note: Some types we can write directly. Note them here:
# > has attribute tag
WRITE_THROUGH_BASE_TYPES <- c(
    "integer", "integer[]", "double", "double[]", "raw", "raw[]", 
    "character", "character[]", "logical", "logical[]", "complex", "complex[]"
)

WRITE_THROUGH_TYPES <- c(
    WRITE_THROUGH_BASE_TYPES, "environment", "pairlist", 
    "symbol", "expression", "language", "...", "any",
    "" # NOTE: Remove this if you want to explicitly have "" be caught. It's not a bug, just
       # a weirdness with how the analysis operates for inner types.
    # "jumped" is for the dispatch cases that unwound
    # , "NULL" ... is not a write-through type anymore
)

TRIVIAL_CONVERSION_TYPES <- c(
    missing = "any",
    `???` = "any",
    NULL = "null",
    LANGSXP = "language",
    S4 = "s4",
    EXTPTRSXP = "externalptr",
    jumped = "any",
    `list<>` = "list<any>",
    `class<\`function\`>` = "any => any"
)

FUNCTION_TYPES <- c(
    "closure",
    "builtin",
    "special",
    "class<function>"
)


# Ok. If we want to compare structs with (no structs and classes), do:
#   one DF just by using this, and one DF by using this after class-ifying the other one, setting
#   the "structless" flag to TRUE
# CURRENTLY: use this function AFTER you have used quick_simplify_types
write_out_a_type <- function(t, structless=FALSE, tupleless=FALSE, extra_simple=FALSE) {
    types_and_tags <- map(t, function(e) top_level_split(e, split="@"))

    map(types_and_tags, function(tat) {
        type <- tat[1]
        tags <- c()
        if (length(tat) > 1)
            tags <- tat[2:length(tat)]

        # Deals with case where the thing is NA...
        if (is.na(type)) {
            NA

        # Deals with write-through types, and puts ! in front of the (primitive) ones
        # if they are NA-free.
        } else if (type %in% WRITE_THROUGH_TYPES) {
            if (type %in% WRITE_THROUGH_BASE_TYPES && ! "NA-free" %in% tags) {
                paste("^", type, sep="")
            } else {
                type
            }
        }

        # For trivial mappings.
        else if (type %in% names(TRIVIAL_CONVERSION_TYPES)) {
            TRIVIAL_CONVERSION_TYPES[[type]]
        }

        # Matrices don't make it through in this phase.
        else if (type %in% paste(PRIM_TYPES, "[,]", sep="")) {
            out <- paste(substr(type, 1, nchar(type)-3), "[]", sep="")
            if (! "NA-free" %in% tags) {
                paste("^", out, sep="")
            } else {
                out
            }
        }

        # Classes
        else if (substr(type, 1, 5) == "class") {
            # Just write it out, as-is. It's already formatted for write.
            if (!extra_simple)
                type
            else   
                "class"
        }

        # Structs
        else if (substr(type, 1, 6) == "struct") {

            if (extra_simple)
                "list"
            else {
                # If we're outputting structs, do this.
                if (!structless) {
                    # Get inner (name, type) pairs.
                    inner <- split_up_struct_names_and_types(type)

                    # Recurse and modify inner types.
                    inner <- map(inner, function(e) {
                        top_level_split(e, ":")
                    })

                    names <- map(inner, function(e) e[1])
                    types <- map(inner, function(e) e[2])
                    types <- map(types, function(t) write_out_a_type(t, structless, tupleless))

                    inner <- map2(names, types, function(n, t) paste(n, t, sep=":"))

                    # Write out {{ ... }}
                    # paste("{{", paste(inner, collapse=", "), "}}", sep = "")
                    paste("struct<", paste(inner, collapse=", "), ">", sep = "")
                
                # Otherwise, we're turning structs into lists.
                } else {
                    # Get inner types.
                    inner <- split_up_struct_names_and_types(type)

                    # Recurse and modify inner types.
                    inner <- map(inner, function(e) {
                        top_level_split(e, ":")
                    })

                    # We only care about types, here.
                    types <- map(inner, function(e) e[2])
                    types <- map(types, function(t) write_out_a_type(t, structless=TRUE, tupleless))

                    # If we're not doing away with tuples:
                    if (!tupleless) {
                        # if (length(types) <= TUPLE_TO_LIST_HEURSTIC) {
                            # paste("[[", paste(map(inner_types, write_out_a_type), collapse=", "), "]]", sep = "")
                            paste("tuple<", paste(types, collapse=", "), ">", sep = "")
                        # } else {
                            # paste("((", consolidate_types_to_one(map(inner_types, write_out_a_type)), "))", sep = "")
                        #     paste("list<", consolidate_types_to_one(types), ">", sep = "")
                        # }

                    # If we are:
                    } else {
                        paste("list<", consolidate_types_to_one(types), ">", sep = "")
                    }
                    
                }
            }  
        }

        # Data.frames
        # Won't be happning with class<data.frame> now.
        else if (substr(type, 1, 4) == "data") {
            "dataframe"

            # Actually, not anymore!
            # Data.frames are like structs in this incarnation.
            # Get inner (name, type) pairs.
            # NOTE: Must do this on "t" since "type" has all tags removed.
            # inner <- split_up_data_frame_names_and_types(t)

            # Recurse and modify inner types.
            # inner <- map(inner, function(e) {
            #     top_level_split(e, ":")
            # })

            # names <- map(inner, function(e) e[1])
            # types <- map(inner, function(e) e[2])
            # types <- map(types, write_out_a_type)

            # inner <- map2(names, types, function(n, t) paste(n, t, sep=":"))

            # Write out {{ ... }}
            # paste("{{", paste(inner, collapse=", "), "}}", sep = "")
            # paste("struct<", paste(inner, collapse=", "), ">", sep = "")
        }

        # Standard lists
        else if (substr(type, 1, 4) == "list") {

            if (extra_simple) {
                "list"
            }
            else {
                # Get inner type. This function also works for lists, it's poorly named.
                inner_types <- split_up_struct_names_and_types(type)

                # If we're not doing away with tuples:
                if (!tupleless) {
                    # if (length(inner_types) <= TUPLE_TO_LIST_HEURSTIC) {
                        # paste("[[", paste(map(inner_types, write_out_a_type), collapse=", "), "]]", sep = "")
                        paste("tuple<", paste(map(inner_types, function(t) write_out_a_type(t, structless, tupleless)), collapse=", "), ">", sep = "")
                    # } else {
                    #     # paste("((", consolidate_types_to_one(map(inner_types, write_out_a_type)), "))", sep = "")
                    #     paste("list<", consolidate_types_to_one(map(inner_types, function(t) write_out_a_type(t, structless, tupleless))), ">", sep = "")
                    # }

                # If we are:
                } else {
                    paste("list<", consolidate_types_to_one(map(inner_types, function(t) write_out_a_type(t, structless, tupleless))), ">", sep = "")
                }
            }
        }

        # Functions
        else if (type %in% FUNCTION_TYPES) {
            # Ok. We want to output a function type here, but we don't know what
            # that type is yet. We can keep the name of the function in the type?
            # And then add a phase after this to take the holes and fill them in.
            # What about circular dependencies though?

            # For now, try to just put the type in. The ORIGINAL one, with
            # the tags.
            # paste0("fn[", tat[2], "]")
            # tat[2]

            # Actually we're NOT doing the tags, just do any => any
            "any => any"
        }

        # Catch-all
        else {
            paste0("{MISSING_CASE: ", type, "}") # this should introduce an error, so that we can deal with it.
        }

    }) %>% unlist # unlist because we end up with unnecessary list wrappers
}

# Ok. ACTUALLY, we want to get types for all function ids.
# Ok. The above writes out a type for a specific argument.
# Now we want to consolidate this to work over an entire function.
# Currently doing this through the data.frame itself.
get_type_of_function <- function(df, pname, f_id, structless=FALSE, tupleless=FALSE, collapse=TRUE, unify_lists=TRUE, primitive_subtyping=TRUE) {
    # First, check if it has_dots.
    # NVM we don't care, we do it argument-by-argument.
    # has_dots <- df %>% filter(package == pname, fun_name == fname) %>% select(has_dots) %>% sum > 0

    call_traces <- df %>% filter(package == pname, fun_id == f_id) %>% select(starts_with("arg_t")) %>% mutate_all(function(t) write_out_a_type(t, structless, tupleless))

    # Consolidate.
    types <- map(names(call_traces), function(n) {
        if (n == "arg_t_r")
            call_traces %>% select(n) %>% unlist %>% unname %>% consolidate_types_to_one(max_num_types = nrow(df) + 1, is_return=TRUE, unify_lists=unify_lists, primitive_subtyping=primitive_subtyping)
        else
            call_traces %>% select(n) %>% unlist %>% unname %>% consolidate_types_to_one(max_num_types = nrow(df) + 1, unify_lists=unify_lists, primitive_subtyping=primitive_subtyping)
    }) %>% unlist

    # Ok. types[1] is the return, the rest are not.
    # NOTE: TODO!! if one of the cols has SOME NAs, need a ?missing qualifier.
    # Don't want NAs.
    types <- types[!is.na(types)]

    # If we want nice collapsed types:
    if (collapse) {
            if (length(types) == 1) {
            # There's just a return value.
            paste("< > => ", types[1], sep="")
        } else {
            # Length is > 1, it's not gonna be empty.
            params <- paste(types[2:length(types)], collapse = ", ")

            # Don't need this anymore.
            # if (has_dots)
            #     params <- paste0(params, ", ...")

            paste("<", params, "> => ", types[1], sep = "")
        }    
    } else {
        names(types) <- names(call_traces)[1:length(types)]
        types
    }
    
}


get_fun_types_for_package <- function(df, pkg, structless=F, tupleless=F, collapse=T, restrict_analysis=F, unify_lists=T,
                                      primitive_subtyping=TRUE, exported_funs=data.frame()) {
  # First, filter out for a particular package.
  if (restrict_analysis)
    df_p <- df %>% filter(package == pkg, package_being_analyzed == pkg)
  else
    df_p <- df %>% filter(package == pkg)
  
  # What are the function ids?
  p_fun_ids <- df_p %>% select(fun_id) %>% unique %>% unlist %>% unname
  p_fun_ids <- p_fun_ids[!is.na(p_fun_ids)]
  
  # Let's build a type for each of these.
  names(p_fun_ids) <- p_fun_ids
  types_by_id <- map(p_fun_ids, function(x) {
    get_type_of_function(df_p, pkg, x, structless, tupleless, collapse, unify_lists=unify_lists, primitive_subtyping=primitive_subtyping)
  })

  # So now we have types per ID. For each of these IDs, we should see how many function names have had it,
  # and generate a type for each of them.
  # In the cases where multiple functions have had some ID, we will simply | the types.

  # Need names for output.
  if (nrow(exported_funs) == 0) {
    p_fun_names <- df_p %>% select(fun_name) %>% unique %>% unlist %>% unname
    p_fun_names <- p_fun_names[!is.na(p_fun_names)]
  } else {
    # There was something passed.
    p_fun_names <- exported_funs$fun_name %>% unlist
  }
  
  types_per_fun_name <- rep(list(list()), length(p_fun_names))
  names(types_per_fun_name) <- p_fun_names

  for (id in p_fun_ids %>% unname) {
      fnames_for_id <- df_p %>% filter(fun_id == id) %>% select(fun_name) %>% unique %>% unlist %>% unname
      
      # Ignore NA names...
      fnames_for_id <- fnames_for_id[!is.na(fnames_for_id)]

      # Only want certain names...
      fnames_for_id <- fnames_for_id[fnames_for_id %in% p_fun_names]

      for (n in fnames_for_id) {
          types_per_fun_name[[n]] <- c(types_per_fun_name[[n]], types_by_id[[id]])
      }
  }

  types_per_fun_name <- types_per_fun_name %>% map(function(types) {paste(types, collapse=" | ")})

  # Don't want the empty ones.
  types_per_fun_name[types_per_fun_name != ""]
}

format_package_type_for_write <- function(lot, remove_fn_id=F) {
    map(names(lot), function(n) {
        if (!remove_fn_id) {
            paste0("type `", n, "` ", lot[[n]]);
        } else {
            theSplit = strsplit(lot[[n]], ":", fixed=T)[[1]]
            paste0("type `", n, "`", paste(theSplit[2:length(theSplit)], collapse=":"));
        }
    })
}

format_package_types_for_write <- function(types) {
    format_package_type_for_write(types) %>% map(function(t) paste0(t, ";")) %>% unlist %>% unname
}


# Do another one, that just unions all of the signatures into one.
get_type_of_function_top_level_union <- function(df, pname, fname) {
    call_traces <- df %>% filter(package == pname, fun_name == fname) %>% select(starts_with("arg_t")) %>% mutate_all(write_out_a_type)

    # Quick trim NA cols to clean up output:
    call_traces <- call_traces %>% quick_trim_na_cols

    call_traces %>% by(1:nrow(call_traces), function(row) {
        row[is.na(row)] <- "???" # Record columns that aren't entirely NA as ???

        paste("<", paste(row[2:length(row)], collapse=", "), ">", "=>", row[1])
    }) -> all_sigs

    paste(all_sigs, collapse=" | ")
}

LGL_SUBTYPE_OF <- c("logical", "integer", "double", "complex")
INT_SUBTYPE_OF <- c("integer", "double", "complex")
DBL_SUBTYPE_OF <- c("double", "complex")
CLX_SUBTYPE_OF <- c("complex")

EASY_SUBTYPES <- list(
    logical = c(LGL_SUBTYPE_OF, paste0("^", LGL_SUBTYPE_OF), paste0(LGL_SUBTYPE_OF, "[]"), paste0("^", LGL_SUBTYPE_OF, "[]")),
    integer = c(INT_SUBTYPE_OF, paste0("^", INT_SUBTYPE_OF), paste0(INT_SUBTYPE_OF, "[]"), paste0("^", INT_SUBTYPE_OF, "[]")),
    double  = c(DBL_SUBTYPE_OF, paste0("^", DBL_SUBTYPE_OF), paste0(DBL_SUBTYPE_OF, "[]"), paste0("^", DBL_SUBTYPE_OF, "[]")),
    complex = c(CLX_SUBTYPE_OF, paste0("^", CLX_SUBTYPE_OF), paste0(CLX_SUBTYPE_OF, "[]"), paste0("^", CLX_SUBTYPE_OF, "[]")),
    `logical[]` = c(paste0(LGL_SUBTYPE_OF, "[]"), paste0("^", LGL_SUBTYPE_OF, "[]")),
    `integer[]` = c(paste0(INT_SUBTYPE_OF, "[]"), paste0("^", INT_SUBTYPE_OF, "[]")),
    `double[]`  = c(paste0(DBL_SUBTYPE_OF, "[]"), paste0("^", DBL_SUBTYPE_OF, "[]")),
    `complex[]` = c(paste0(CLX_SUBTYPE_OF, "[]"), paste0("^", CLX_SUBTYPE_OF, "[]")),
    `^logical` = c(paste0("^", LGL_SUBTYPE_OF), paste0("^", LGL_SUBTYPE_OF, "[]")),
    `^integer` = c(paste0("^", INT_SUBTYPE_OF), paste0("^", INT_SUBTYPE_OF, "[]")),
    `^double`  = c(paste0("^", DBL_SUBTYPE_OF), paste0("^", DBL_SUBTYPE_OF, "[]")),
    `^complex` = c(paste0("^", CLX_SUBTYPE_OF), paste0("^", CLX_SUBTYPE_OF, "[]")),
    `^logical[]` = c(paste0("^", LGL_SUBTYPE_OF, "[]")),
    `^integer[]` = c(paste0("^", INT_SUBTYPE_OF, "[]")),
    `^double[]`  = c(paste0("^", DBL_SUBTYPE_OF, "[]")),
    `^complex[]` = c(paste0("^", CLX_SUBTYPE_OF, "[]")),
    character = c("^character", "character[]", "^character[]"),
    raw = c("^raw", "raw[]", "^raw[]"),
    `character[]` = c("character[]", "^character[]"),
    `raw[]` = c("raw[]", "^raw[]"),
    `^character` = c("^character", "^character[]"),
    `^raw` = c("^raw", "^raw[]"),
    `^character[]` = c("^character[]"),
    `^raw[]` = c("^raw[]")
)

EASY_SUBTYPES_NO_CROSS_SUBTYPING <- list(
    logical = c("logical", "logical[]", "^logical", "^logical[]"),
    integer = c("integer", "integer[]", "^integer", "^integer[]"),
    double  = c("double", "double[]", "^double", "^double[]"),
    complex = c("complex", "complex[]", "^complex", "^complex[]"),
    character = c("^character", "character[]", "^character[]"),
    raw = c("^raw", "raw[]", "^raw[]"),
    `logical[]` = c("logical[]", "^logical[]"),
    `integer[]` = c("integer[]", "^integer[]"),
    `double[]`  = c("double[]", "^double[]"),
    `complex[]` = c("complex[]", "^complex[]"),
    `character[]` = c("character[]", "^character[]"),
    `raw[]` = c("raw[]", "^raw[]"),
    `^logical` = c("^logical", "^logical[]"),
    `^integer` = c("^integer", "^integer[]"),
    `^double`  = c("^double", "^double[]"),
    `^complex` = c("^complex", "^complex[]"),
    `^logical[]` = "^logical[]",
    `^integer[]` = "^integer[]",
    `^double[]`  = "^double[]",
    `^complex[]` = "^complex[]",
    `^character` = c("^character", "^character[]"),
    `^raw` = c("^raw", "^raw[]"),
    `^character[]` = c("^character[]"),
    `^raw[]` = c("^raw[]")
)

# Takes a string s of the form: `name`:type.
# Idea: skip over `...`, find :
split_up_name_type_pairs <- function(s) {
    parsing_name <- FALSE

    for (i in 1:nchar(s)) {
        the_char <- substr(s, i, i)

        if (the_char == "`") {
            parsing_name <- !parsing_name
        } else if (the_char == ":" && !parsing_name) {
            # This is the one we want.
            return(c(substr(s, 1, i-1), substr(s, i+1, nchar(s))))
        }
    }

    c()
}

is_tuple_subtype <- function(t1, t2) {
    if (substr(t1, 1, 4) != substr(t2, 1, 4))
        return(FALSE)
    else if (substr(t1, 1, 4) != "list") {
        return(FALSE)
    }

    t1s <- split_up_struct_names_and_types(t1)
    t2s <- split_up_struct_names_and_types(t2)

    if (length(t1s) != length(t2s))
        return(FALSE)

    # By now, if we haven't returned, they are both tuples.
    # Tuples will not have width subtying, but will have depth.
    # TODO: should it have depth?
    map2(t1s, t2s, is_subtype) %>% reduce(`&&`)
}

# is_data_frame_subtype_pre_simpl <- function(t1, t2) {
#    if (substr(t1, 1, 4) != substr(t2, 1, 4))
#         return(FALSE)
#     else if (substr(t1, 1, 4) != "data") {
#         return(FALSE)
#     } 

#     t1s <- split_up_data_frame_names_and_types(t1)
#     t2s <- split_up_data_frame_names_and_types(t2)

#     is_width_and_depth_subtype_pre_simpl(t1s, t2s)
# }

# is_struct_subtype_pre_simpl <- function(t1, t2) {
#     if (substr(t1, 1, 6) != substr(t2, 1, 6))
#         return(FALSE)
#     else if (substr(t1, 1, 6) != "struct") {
#         return(FALSE)
#     }

#     # TODO: There has to be a more efficient way to do this.
#     t1s <- split_up_struct_names_and_types(t1)
#     t2s <- split_up_struct_names_and_types(t2)

#     is_width_and_depth_subtype_pre_simpl(t1s, t2s)
# }

# is_width_and_depth_subtype_pre_simpl <- function(t1s, t2s) {
#     t1s <- map(t1s, split_up_name_type_pairs)
#     t2s <- map(t2s, split_up_name_type_pairs)

#     t1_names <- map(t1s, function(x) x[[1]])
#     t2_names <- map(t2s, function(x) x[[1]])

#     # First of all, if t2 has any names that t1 doesn't, it's not a subtype.
#     # t1 has to have at least everything in t2.
#     # Width subtyping.
#     if (!reduce(t2_names %in% t1_names, `&&`)) {
#         return(FALSE)
#     }

#     t1_types <- map(t1s, function(x) x[[2]])
#     t2_types <- map(t2s, function(x) x[[2]])

#     names(t1_types) <- t1_names
#     names(t2_types) <- t2_names

#     # Ok. By now we know that t1 has all of t2's names.
#     # Depth subtyping time!
#     for (t2n in t2_names) {
#         if (!is_subtype(t1_types[[t2n]], t2_types[[t2n]]))
#             return(FALSE)
#     }

#     # If we get to this point, we've exhausted all possible issues.
#     TRUE
# }

is_struct_subtype <- function(t1, t2, primitive_subtyping=TRUE) {
    if (substr(t1, 1, 2) != substr(t2, 1, 2))
        return(FALSE)
    # else if (substr(t1, 1, 2) != "{{") {
    else if (substr(t1, 1, 2) != "st") {
        return(FALSE)
    } 

    t1s <- split_up_names_and_types(t1)
    t2s <- split_up_names_and_types(t2)

    is_width_and_depth_subtype(t1s, t2s, primitive_subtyping=primitive_subtyping)
}

is_list_subtype <- function(t1, t2, primitive_subtyping=TRUE) {
    if (substr(t1, 1, 2) != substr(t2, 1, 2))
        return(FALSE)
    # else if (substr(t1, 1, 2) != "((" && substr(t1, 1, 2) != "[[") {
    else if (substr(t1, 1, 2) != "li" && substr(t1, 1, 2) != "tu") {
        return(FALSE)
    }

    # TODO: There has to be a more efficient way to do this.
    t1s <- split_up_names_and_types(t1)
    t2s <- split_up_names_and_types(t2)

    is_width_and_depth_subtype(t1s, t2s, primitive_subtyping=primitive_subtyping)
}

is_width_and_depth_subtype <- function(t1s, t2s, primitive_subtyping=TRUE) {
    # First of all, if t2 has any names that t1 doesn't, it's not a subtype.
    # t1 has to have at least everything in t2.
    # Width subtyping.
    if (!reduce(names(t2s) %in% names(t1s), .init=TRUE, `&&`)) {
        return(FALSE)
    }

    # Generally, if the first is shorter, that's bad.
    if (length(t1s) < length(t2s))
        return(FALSE)

    # Ok. By now we know that t1 has all of t2's names.
    # Depth subtyping time!
    # IF THERE ARE NAMES:

    if (!is.null(names(t1s)) && !is.null(names(t2s))) {

        for (t2n in names(t2s)) {

            # This catches cases where not all of the struct elements are named.
            # It handles NA and "", which may be introduced as part of the
            # data processing pipeline (as part of sanitization).
            if (is.na(t2n) | t2n == "")
                next()

            if (!is_subtype(t1s[[t2n]], t2s[[t2n]], primitive_subtyping=primitive_subtyping))
                return(FALSE)
        }
    } else { 
        # Names are NULL, proceed elementwise.
        for (i in 1:min(length(t1s), length(t2s))) {
            if (!is_subtype(t1s[[i]], t2s[[i]], primitive_subtyping=primitive_subtyping))
                return(FALSE)
        }
    }

    # If we get to this point, we've exhausted all possible issues.
    TRUE
}

# Subtypes:
# 1. primitive subtying rules, see above
# 2. tuples: need to be same. ppl can do l[length(l)] or smth
# 3. structs: width and depth
# 4. T <: any \forall T
# There's not too much else. NULLs should be handled separately, since we have a separate null-able annotation.
# is_subtype_pre_simpl <- function(t1, t2) {
#     # In some cases, dispatched functions don't pick up on outer functions
#     # having a vararg. Either way, if one of the types we are consolidating
#     # is ..., let's subsume all others.
#     if (t1 == t2) {
#         return(TRUE)
#     } 
#     # If t2 is ? t2', then we want to compare t1 to t2' (as NULL-ness of )
#     if (substr(t2, 1, 1) == "?" && substr(t2, 2, 2) == " ") {
#         t2 <- substr(t2, 3, nchar(t2))
#     }

#     if (t2 == "any") { # || t2 == "...") { 
#         TRUE
#     } else if (t1 %in% names(EASY_SUBTYPES)) {
#         t2 %in% EASY_SUBTYPES[[t1]]
#     } else if (substr(t1, 1, 4) == "list") {
#         is_tuple_subtype(t1, t2)
#     } else if (substr(t1, 1, 6) == "struct") {
#         is_struct_subtype(t1, t2)
#     } else if (substr(t1, 1, 4) == "data") {
#         is_data_frame_subtype(t1, t2)
#     } else {
#         FALSE
#     }
# }

is_subtype <- function(t1, t2, primitive_subtyping=TRUE) {
    # In some cases, dispatched functions don't pick up on outer functions
    # having a vararg. Either way, if one of the types we are consolidating
    # is ..., let's subsume all others.
    # Actually, this shouldn't happen.
    # Counting "..." as any, to absorb dispatch weirdness.
    if (t2 == "any" || t2 == "..." || t2 == "unused" || t2 == "missing" || t1 == t2) { # || t2 == "...") { 
        return(TRUE)
    } 

    # If t2 is ? t2', then we want to compare t1 to t2' (as NULL-ness of )
    if (substr(t2, 1, 2) == "? ") {
        t2 <- substr(t2, 3, nchar(t2))
    }
    
    if (t1 == t2) {
        TRUE
    } else if (primitive_subtyping & t1 %in% names(EASY_SUBTYPES)) {
        t2 %in% EASY_SUBTYPES[[t1]]
    } else if (!primitive_subtyping & t1 %in% names(EASY_SUBTYPES)) {
        t2 %in% EASY_SUBTYPES_NO_CROSS_SUBTYPING[[t1]]
    } else if (substr(t1, 1, 2) == "li" || substr(t1, 1, 2) == "tu") {
        is_list_subtype(t1, t2, primitive_subtyping=primitive_subtyping)
    # } else if (substr(t1, 1, 2) == "{{") {
    } else if (substr(t1, 1, 2) == "st") {
        is_struct_subtype(t1, t2, primitive_subtyping=primitive_subtyping)
    } else {
        FALSE
    }
}


unify_class_types <- function(lot) {
    types <- substr(lot, 7, nchar(lot) - 1)
    class_names <- strsplit(types, split=", ", fixed=T) %>% reduce(c) %>% unique
    paste0("class<", paste(class_names, collapse=", "), ">")
}

unify_structs_lists_and_tuples <- function(lot) {
    # The idea with this function is to take many struct, list, and tuple types
    # and consolidate them into their logical supertype.
    # General idea: structs and tuples exist only when they are used consistently.
    # So, we want to promote them to a list type when consistency is lost.
    # Roughly, the rules are as follows:
    # unify(tuple, list) => list
    # unify(struct, list) => list
    # unify(struct, tuple) => list
    # unify(struct<a, b>, struct<a, c>) => struct<a>
    # unify(struct<a>, struct<b>) => list
    # Three-pronged approach.
    # 1. Top level types:
    # a. Pick out the lists first, we deal with them separately.
    lists_only <- lot[substr(lot, 1, 4) == "list"]
    tas <- lot[substr(lot, 1, 4) != "list"]

    # Ignore empty lists, structs, and tuples?
    lists_only <- lists_only[lists_only != "list<>"]
    tas <- tas[tas != "struct<>" & tas != "tuple<>"]

    if (length(lists_only) == 0 && length(tas) == 0) {
        return("list<any>") # make this list<> if we leave tuples behind
    }

    # b. If there are any lists, we just want the types from the non-list elements to try to make a smart
    #    overall type.
    if (length(lot) == 1) {
        lot
    } else if (length(lists_only) > 0 && length(tas) > 0) {
        # Get types from tas.
        tas_types <- map(tas, function(t) split_up_names_and_types(t) %>% unname)
        # And also from the lists.
        l_types <- map(lists_only, split_up_names_and_types) %>% unlist

        # We should consolidate all of these types into one.
        descriptive_type <- consolidate_types_to_one(c(reduce(tas_types, c), l_types))

        # Finally, return.
        paste0("list<", descriptive_type, ">")
    } else if (length(lists_only) > 0 && length(tas) == 0) {
        # There were lists, and no structs.
        l_types <- map(lists_only, split_up_struct_names_and_types) %>% unlist

        # We should consolidate all of these types into one.
        descriptive_type <- consolidate_types_to_one(l_types)

        # Finally, return.
        paste0("list<", descriptive_type, ">")
    } else if (length(lists_only) == 0 && length(tas) > 0) {
        # Final case, where there are no big lists, only structs and tuples.
        # First, deal with case where there are tuples and structs.
        tuples <- tas[substr(tas, 1, 5) == "tuple"]
        structs <- tas[substr(tas, 1, 6) == "struct"]

        if (length(tuples) > 0 && length(structs) > 0) {
            # There are both.
            tuple_types <- map(tuples, split_up_names_and_types)
            struct_types <- map(structs, function(t) split_up_names_and_types(t) %>% unname)
        
            descriptive_type <- consolidate_types_to_one(c(reduce(tuple_types, c), reduce(struct_types, c)))

            # We could do more sophisticated processing here, but the intent with tuples and structs was that
            # they would be used consistently. We're just going to produce a list type here.
            paste0("list<", descriptive_type, ">")
        } else if (length(tuples) > 0 && length(structs) == 0) {
            # Just tuples.
            tuple_types <- map(tuples, split_up_names_and_types)

            # If they all have the same length, we could try elementwise composition, provided that the types are somewhat compatible.
            # But maybe we punt on that for now.
            descriptive_type <- consolidate_types_to_one(reduce(tuple_types, c))

            paste0("list<", descriptive_type, ">")
        } else if (length(tuples) == 0 && length(structs) > 0) {
            # The final case, only structs here. 
            # We have a few unification strategies in mind. We would like elementwise type consolidation (for similar names),
            # and taking a superset of the names.
            struct_names_and_types <- map(structs, function(t) split_up_names_and_types(t))

            # Grab common names.
            common_names <- map(struct_names_and_types, names) %>% reduce(intersect)

            # If there are *no* common names, we want to do something special, and say it's just a list with the most appropriate type:
            if (length(common_names) == 0) {
                descriptive_type <- consolidate_types_to_one(reduce(struct_names_and_types %>% map(unname), c))
                paste0("list<", descriptive_type, ">")
            } else {
                # Reduce struct_names_and_types to only those in the common names.
                struct_names_and_types <- map(struct_names_and_types, function(l) l[names(l) %in% common_names])

                new_struct <- c()
                # Name-wise, let's consolidate the types.
                for (n in common_names) {
                    # For each struct in struct_names_and_types, get the relevant name, consolidate.
                    name_types <- map(struct_names_and_types, function(s) s[n]) %>% unlist %>% unname
                    the_type <- consolidate_types_to_one(name_types)
                    new_struct <- c(new_struct, structure(the_type, names=n))
                }

                # Remove empty names...
                new_struct <- new_struct[names(new_struct) != ""]

                the_names <- names(new_struct)
                new_struct <- new_struct %>% unname
                
                paste0("struct<", map2(the_names, new_struct, function(n, t) { paste0(n, ":", t) }) %>% paste(collapse=", "), ">")
            }
        }
    }
}

remove_redundant_types <- function(lot, primitive_subtyping=TRUE) {
    lot <- unique(lot)

    # For each type t1 in lot, if \exists another type t2 in lot s.t. t1 <: t2 and t1 <> t2, remove t1.
    # This is so slow. Is there a better way to do this, to speed it up?
    subtypes <- map(lot, function(t1) {
        map(lot, function(t2) {
            t1 != t2 && is_subtype(t1, t2, primitive_subtyping=primitive_subtyping)
        }) %>% reduce(`||`)
    }) %>% unlist

    # %>% unique gets rid of unwanted duplicates.
    lot[!subtypes] %>% unique
}

consolidate_types_to_one <- function(lot, max_num_types = LIST_UNION_HEURISTIC, is_return=FALSE, unify_lists=TRUE, primitive_subtyping=TRUE) {
    lot <- lot[!is.na(lot)]

    # There are no non-NAs if this is true.
    if (length(lot) == 0)
        return(NA)

    # Do subtyping first. In case we have e.g. list of integers and doubles.
    trimmed_types <- remove_redundant_types(lot, primitive_subtyping=primitive_subtyping)

    # Short-circuit for only 1 thing.
    if (length(trimmed_types) == 1)
        return(trimmed_types[[1]])

    if (unify_lists) {
        # Ok. Let's unify lists and structs and tuples.
        # First, move the list types out.
        substrs <- substr(trimmed_types, 1, 4)
        lst_types <- trimmed_types[substrs == "stru" | substrs == "list" | substrs == "tupl"]
        # For classes:
        # class_types <- trimmed_types[substrs == "clas"]
        trimmed_types <- trimmed_types[! (substrs == "stru" | substrs == "list" | substrs == "tupl")] # | substrs == "clas")]

        # Next, unify the list types, and add them back.
        if (length(lst_types) > 0) { 
            unified_list_type <- unify_structs_lists_and_tuples(lst_types)
            trimmed_types <- c(trimmed_types, unified_list_type)
        }

        # if (length(class_types) > 0) {
        #     trimmed_types <- c(trimmed_types, unify_class_types(class_types))
        # }
    }

    # if (length(trimmed_types) <= max_num_types) {
        # Make a union.

        if (is_return) {
            if (length(trimmed_types) > 1)
                paste0("(", paste(trimmed_types, collapse=" | "), ")")
            else
                paste(trimmed_types, collapse=" | ")
        } else {
            paste(trimmed_types, collapse=" | ")
        }
    # } else {
    #     "any"
    # }
}

# make_stupidly_simple_type <- function(t) {
#     # types_and_tags <- map(t, function(e) top_level_split(e, split="@"))
#     strsplit(t, "@", fixed=T) %>%
#     map(function(x) x[[1]]) %>% unlist %>%
#     strsplit("<", fixed=T) %>% 
#     map(function(x) x[[1]]) %>% unlist
# }

# compute_subtyping_hash_table <- function(list_of_types) {
#     # Build list of hashes, and link them to the types.
#     hashes <- map(list_of_types, digest) %>% unlist
#     names(hashes) <- list_of_types

#     # Do the subtying.
#     subtypes <- rep(list(list()), length(list_of_types))
#     names(subtypes) <- hashes %>% unlist %>% unname

#     for (i in 1:length(list_of_types)) {
#         for (j in 1:length(list_of_types)) {
#             # cat("comparing ", i, " with ", j, "...\n");
#             if (is_subtype(list_of_types[[i]], list_of_types[[j]])) {
#                 cat("subtype between ", i, " and ", j, "\n")
#                 subtypes[[hashes[[list_of_types[[i]]]]]] <- c(subtypes[[hashes[[list_of_types[[i]]]]]], hashes[[list_of_types[[j]]]])
#             }
#         }
#     }

#     list(hashes=hashes, subtype_hashtable=subtypes)
# }

process_class_to_type <- function(c) {
    # Take a {a-b-c} and make it class<a, b, c>.
    c <- substr(c, 2, nchar(c) - 1)
    c <- paste0("class<", map(strsplit(c, "-"), function(s) paste(s, collapse=", ")), ">")
    c
}

quick_simplify_df_to_class_and_list <- function(df) {
    # Ok. What do we want to do here? 
    # If for some row there's a class that's not {}, we want that to become the type.
    # We will need to adjust MISSING_CASE above to not be missing classes.

    # Do this for every arg_t:
    df <- df %>% mutate(arg_t_r = ifelse(arg_c_r == "{}", arg_t_r, process_class_to_type(arg_c_r)),
                        arg_t0 = ifelse(arg_c0 == "{}", arg_t0, process_class_to_type(arg_c0)),
                        arg_t1 = ifelse(arg_c1 == "{}", arg_t1, process_class_to_type(arg_c1)),
                        arg_t2 = ifelse(arg_c2 == "{}", arg_t2, process_class_to_type(arg_c2)),
                        arg_t3 = ifelse(arg_c3 == "{}", arg_t3, process_class_to_type(arg_c3)),
                        arg_t4 = ifelse(arg_c4 == "{}", arg_t4, process_class_to_type(arg_c4)),
                        arg_t5 = ifelse(arg_c5 == "{}", arg_t5, process_class_to_type(arg_c5)),
                        arg_t6 = ifelse(arg_c6 == "{}", arg_t6, process_class_to_type(arg_c6)),
                        arg_t7 = ifelse(arg_c7 == "{}", arg_t7, process_class_to_type(arg_c7)),
                        arg_t8 = ifelse(arg_c8 == "{}", arg_t8, process_class_to_type(arg_c8)),
                        arg_t9 = ifelse(arg_c9 == "{}", arg_t9, process_class_to_type(arg_c9)),
                        arg_t10 = ifelse(arg_c10 == "{}", arg_t10, process_class_to_type(arg_c10)),
                        arg_t11 = ifelse(arg_c11 == "{}", arg_t11, process_class_to_type(arg_c11)),
                        arg_t12 = ifelse(arg_c12 == "{}", arg_t12, process_class_to_type(arg_c12)),
                        arg_t13 = ifelse(arg_c13 == "{}", arg_t13, process_class_to_type(arg_c13)),
                        arg_t14 = ifelse(arg_c14 == "{}", arg_t14, process_class_to_type(arg_c14)),
                        arg_t15 = ifelse(arg_c15 == "{}", arg_t15, process_class_to_type(arg_c15)),
                        arg_t16 = ifelse(arg_c16 == "{}", arg_t16, process_class_to_type(arg_c16)),
                        arg_t17 = ifelse(arg_c17 == "{}", arg_t17, process_class_to_type(arg_c17)),
                        arg_t18 = ifelse(arg_c18 == "{}", arg_t18, process_class_to_type(arg_c18)),
                        arg_t19 = ifelse(arg_c19 == "{}", arg_t19, process_class_to_type(arg_c19)))

    df
}
