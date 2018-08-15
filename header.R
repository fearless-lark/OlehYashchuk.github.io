library(DBI)
library(tictoc)
library(bigrquery)

library(magrittr)
library(data.table)
library(lubridate)
library(purrr)

library(rvest)
library(jsonlite)

library(ggthemes)
library(ggplot2)

library(knitr)
library(sjPlot)

library(caret)

# library(tidyverse) # dbplyr-ggplot2-tibble-readr-tidyr-purr-stringr-forcats

# periods -----------------------------------------------------------------
# All periods of action for every cohort from started date
# periods <- function(start_date, period) {
# 
#     if (period == "month") {
#         start_date <- start_date %>% as.Date()
#     } else if (period == "week") {
#         start_date  <- start_date %>% ymd() %>%
#             floor_date(., unit = "week", week_start = 1) %>% as.Date()
#     }
# 
#     periods <- seq(start_date, Sys.Date() + 365, by = paste("1", period))
#     periods <- periods %>% as.list()
#     names(periods) <- periods
#     periods <- lapply(periods,
#                       function(x) seq(x, Sys.Date() + 365,
#                                       by = paste("1", period)))
#     periods <- lapply(periods,
#                       function(x) data.table(created = min(x), action = x))
#     periods <- do.call(rbind.data.frame, periods)
# 
#     # periods <- periods[created != action, ]
# 
#     setkey(periods, "created", "action")
#     periods[, Period_number := 0]
#     periods[!(created == action), Period_number := 1]
#     periods[, Period_number := cumsum(Period_number), by = list(created)]
# 
#     periods[, period := period]
# 
#     return(periods)
# }

# repeat_last -------------------------------------------------------------
# Support function that fills NA values in table columns
repeat_last <- function(x, forward = TRUE, maxgap = Inf, na.rm = FALSE) {
    if (!forward) x = rev(x)           # reverse x twice if carrying backward
    ind = which(!is.na(x))             # get positions of nonmissing values
    if (is.na(x[1]) && !na.rm)         # if it begins with NA
        ind = c(1,ind)                   # add first pos
    rep_times = diff(           # diffing the indices + length yields how often
        c(ind, length(x) + 1) )          # they need to be repeated
    if (maxgap < Inf) {
        exceed = rep_times - 1 > maxgap  # exceeding maxgap
        if (any(exceed)) {               # any exceed?
            ind = sort(c(ind[exceed] + 1, ind))      # add NA in gaps
            rep_times = diff(c(ind, length(x) + 1) ) # diff again
        }
    }
    x = rep(x[ind], times = rep_times) # repeat the values at these indices
    if (!forward) x = rev(x)           # second reversion
    x
}

# adding_missing_variables ------------------------------------------------
adding_missing_variables <- function(dt, variables, class = "int") {
    for (var in variables) {
        if (!(var %in% names(dt))) {
            if (class %in% c("i", "int", "integer")) {
                dt[, (var) := integer()]
            } else if (class %in% c("c", "char", "character")) {
                dt[, (var) := character()]
            } else if (class %in% c("d", "date")) {
                dt[, (var) := as.Date(character())]
            } else if (class %in% c("n", "num", "numeric")) {
                dt[, (var) := numeric()]
            }
        }
    }
}

# environment objects size ------------------------------------------------
environment_objects_size <- function(environment = .GlobalEnv, units = "MB") {
    if (units == "GB") {
        denom <- 10^9
        print("Units: GB")
    } else {
        denom <- 10^6
        print("Units: MB")
    }

    ls(envir = environment) %>%
        sapply(., function(x) pryr::object_size(get(x))) %>%
        sort()/denom %>%
        return()
}

# number of NAs
num_NAs <- function(df) {
    df %>% map_int(., function(x) is.na(x) %>% sum())
    # df %>% sapply(., function(x) is.na(x) %>% sum())
}
