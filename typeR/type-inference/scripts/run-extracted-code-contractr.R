#!/usr/bin/env Rscript

# NOTE: this file is used for coverage, should not have
# any dependencies except for base R with the exception
# of rapr

library(contractr)


options(
  runr.run_before=function(package, file, type) {
    set_severity("warning")
    clear_contracts()
  },
  runr.run_after=function(package, file, type) {
    contracts <- get_contracts()
    message(nrow(contracts))
    if(nrow(contracts) != 0) {
      library(dplyr)

      contracts <- cbind(
        running_package = package,
        type = type,
        file = file,
        contracts
      )

      all <- select(
        contracts,
        package_name,
        function_name,
        parameter_position,
        actual_type,
        expected_type,
        assertion_status
      )

      failed <- filter(
        contracts,
        !assertion_status
      ) %>% select(
        -call_trace
      )

      dir <- basename(file)
      dir.create(dir, showWarnings=FALSE)
      readr::write_csv(all, file.path(dir, "assertions-all.csv"))
      readr::write_csv(failed, file.path(dir, "assertions-failed.csv"))
    }
  }
)

script <- system.file("tasks/run-extracted-code.R", package="runr")

sys.source(script, envir=new.env())

