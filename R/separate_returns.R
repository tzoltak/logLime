#' @title Identifying and labelling returns to survey screens
#' @description Function is called internally by [separate_logdata_types] and
#' is not exported (i.e. it is not intended to be called by package users
#' themselves). Function creates additional column `entry` that describes which
#' time a given screen was visited by a given respondent while a given event
#' occurred.
#' @inheritParams preprocess_actions
#' @param screenReturnThreshold (Applicable only if `separateReturns != "no"`)
#' The smallest number of milliseconds between the *pageLoaded* event directly
#' following the *submit* event that will be interpreted as indicating returning
#' to a given survey screen after visiting some other screen. If set to `Inf`,
#' returns will be identified only by looking on the sequence of screen visits
#' (this have limitations if paradata is not collected on some survey screens)
#' - see *Details* section below.
#' @details
#' Identification of returns may be performed in two ways (each with its own
#' limitations):
#' \itemize{
#'   \item{By looking at the sequences of screens as they appear in a log-stream
#'         ordered by time stamps. However, these method works reliably only for
#'         survey screens which are surrounded by another survey screens
#'         collecting the log-data (and more generally, only if all the survey
#'         screens collect log-data - if respondents can navigate to any survey
#'         screen at any time). Otherwise it will be possible to visit
#'         another screen without leaving a sign of if in the log-data.}
#'   \item{By looking at the pairs of directly following each other *submit*
#'         and *pageLoaded* events. The problem here is that if *LimeSurvey*
#'         encounters some incorrectly filled up questions while respondent is
#'         trying to follow to the next survey screen, it reloads the page.
#'         This results in recording the same sequence of the *submit* event
#'         directly followed by the *pageLoaded* event as if respondent has
#'         actually left the survey screen and returned. One may try to
#'         distinguish between these situations by using a **heuristic** -
#'         assuming that a very short time (shorter than the one specified by
#'         `screenReturnThreshold`) will indicate reloading the page and longer
#'         times indicate actual leaving and returning to a give survey screen.}
#' }
#' By default the function uses both approaches described above. The second one
#' may be turned off by giving `screenReturnThreshold = Inf`.
#' @importFrom dplyr .data %>% across arrange filter group_by lag mutate pull
#' select summarise
separate_returns <- function(logData, respId, screenId,
                             screenReturnThreshold = 1000) {
  if (!all(c("pageLoaded", "submit") %in% logData$type)) {
    stop("Separating returns to survey screens is not possible for log-data collected using 'logdataLimeSurvey' applet in versions earlier than 1.1.")
  }
  stopifnot(all(pull(logData, {{screenId}}) != "___artificialPseudoScreen___"))
  # there will be probably some warnings in conversion to numerics because of
  # some broken records - that's why suppressWarnings()
  logData <- suppressWarnings(
    logData %>%
      mutate(rowPosition = seq_len(nrow(logData)),
             timeStamp = as.numeric(.data$timeStamp)))
  submitLoad <- logData %>%
    select({{respId}}, {{screenId}}, "type", "timeStamp", "rowPosition") %>%
    filter(.data$type %in% c("submit", "pageLoaded")) %>%
    group_by(across(c({{respId}}, {{screenId}}))) %>%
    mutate(submitNumber = cumsum(.data$type == "submit")) %>%
    filter(.data$submitNumber > 0L) %>%
    group_by(across(c({{respId}}, {{screenId}}, "submitNumber"))) %>%
    summarise(submitLoaded = (.data$rowPosition[.data$type == "pageLoaded"][1L] -
                                .data$rowPosition[.data$type == "submit"][1L]) == 1L,
              timeStampDiff = .data$timeStamp[.data$type == "pageLoaded"][1L] -
                .data$timeStamp[.data$type == "submit"][1L],
              timeStamp = .data$timeStamp[.data$type == "submit"][1L] + 0.5,
              type = "artificialScreen",
              .groups = "drop") %>%
    filter(.data$submitLoaded %in% TRUE,
           .data$timeStampDiff > 1000) %>%
    mutate(across({{screenId}}, ~ "___artificialPseudoScreen___")) %>%
    select({{respId}}, {{screenId}}, "type", "timeStamp")

  logData %>%
    bind_rows(submitLoad) %>%
    arrange(across(c({{respId}}, "timeStamp"))) %>%
    group_by(across({{respId}})) %>%
    mutate(change = .data$screen != lag(.data$screen),
           change = ifelse(is.na(.data$change), TRUE, .data$change),
           entry = cumsum(ifelse(.data$timeStamp >= 0, .data$change, FALSE))) %>%
    arrange(.data$rowPosition) %>%
    mutate(entry = cummax(.data$entry)) %>%
    group_by(across(c({{respId}}, {screenId}))) %>%
    mutate(entry = as.integer(factor(.data$entry))) %>%
    ungroup() %>%
    filter(!is.na(.data$rowPosition)) %>%
    select({{respId}}, {{screenId}}, "timeStamp", "entry", everything(),
           -"rowPosition", -"change") %>%
    return()
}
