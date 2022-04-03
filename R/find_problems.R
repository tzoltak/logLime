#' @title Marking problems in data regarding events
#' @description Function is called internally by
#' \code{\link{separate_logdata_types}} and is not exported (i.e. it is not
#' intended to be called by package users themselves). It finds problems in data
#' and returns data in which observation is a respondent-screen with additional
#' columns marking whether a given problem occurred.
#' @param actions data frame storing data regarding events as returned by
#' \code{\link{preprocess_actions}}
#' @param respScreenIds <tidy-select> names of columns identifying
#' respondent-screens
#' @return data frame with columns:
#' \itemize{
#'   \item{Columns defined by \code{respScreenIds}}
#'   \item{problemsLeftBrowser - whether respondent left browser window (card)
#'         while answering a given screen,}
#'   \item{problemsResized - whether respondent changed the size of a browser
#'         window while answering a given screen,}
#'   \item{problemsNoPageLoaded - whether log-data for a given respondent-screen
#'         does not contain a \emph{pageLoaded} event (if so, it is most
#'         probably because an old version of the JavaScript applet - that not
#'         recorded this type of events - was used to collect the log-data),}
#'   \item{problemsTimeStamps - whether there is a discontinuity in values of
#'         time-stamps for a given respondent-screen that results in negative
#'         duration of \emph{mousemove} events (if so, it is most probably
#'         because an old version of the JavaScript applet - that used
#'         an unreliable method of getting time-stamps - was used to collect
#'         the log-data).}
#' }
#' @importFrom dplyr %>% .data across group_by summarise
find_problems <- function(actions, respScreenIds) {
  actions %>%
    group_by(across({{respScreenIds}})) %>%
    summarise(problemsAnyBroken = as.integer(any(.data$broken %in% 1)),
              problemsLeftBrowser = as.integer(any(.data$type %in% "blur")),
              problemsResized = as.integer(any(.data$type %in% "resize")),
              problemsNoPageLoaded = as.integer(!any(.data$type %in% "pageLoaded")),
              problemsTimeStamps = as.integer(any(.data$duration < 0 & !is.na(.data$duration))),
              .groups = "drop") %>%
    return()
}
