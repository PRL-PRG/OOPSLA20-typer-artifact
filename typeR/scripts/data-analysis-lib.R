
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

BASE_DATA_PRIMITIVE_NAMES <- c("integer", "double", "complex", "logical", "raw", "character")

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

quick_gather_types <- function(df) {
    df %>% select(count, starts_with("arg_t")) %>% gather("pos", "type", -count) %>% filter(!is.na(type))
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

my_read_csv <- function(path) {
    f <- file(path)
    l <- readLines(f, 1)
    close(f)

    ncol <- length(strsplit(l, ",", fixed=T)[[1]])

    col_types <- paste0("cccccl", paste(rep("c", ncol - 7), collapse=""), "i")

    read_csv(path, col_types = col_types)
}

# + 1 for argument type
quick_trim_df_to_x_args <- function(df, how_many = 20) {
    df[,1:min((NUM_PREAMBLE + 3*(how_many+1)), ncol(df))]
}

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
