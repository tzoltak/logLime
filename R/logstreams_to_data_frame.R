#' @title Transforming a data frame with log-data streams to a data frame in
#' a long form
#' @description Function is called internally by
#' \code{\link{separate_logdata_types}} and is not exported (i.e. it is not
#' intended to be called by package users themselves). It pivots columns of
#' a data frame storing log-data streams to a \emph{long} format and then
#' separates log-data streams into different rows and columns. It performs
#' \strong{no} checks or additional transformations (like assuring formats of
#' specific columns) - this is done only afterwards by another functions.
#' @param logData data frame with log-data streams recorded by the
#' \emph{logDataLimeSurvey} applet stored in its columns
#' @param respId <tidy-select> name(s) of the column(s) containing respondent's
#' id (and perhaps other variables you want to keep joined with your log-data)
#' @param questionNamesTo string - name of the variable that will identify
#' survey screen (page) in the results (i.e. one storing names of variables
#' storing log-data streams in the provided data)
#' @return data frame with columns:
#' \itemize{
#'   \item{timeStamp,}
#'   \item{type,}
#'   \item{target.tagName,}
#'   \item{target.id,}
#'   \item{target.class,}
#'   \item{which,}
#'   \item{metaKey,}
#'   \item{pageX,}
#'   \item{pageY,}
#'   \item{broken.}
#' }
#' @importFrom tidyr pivot_longer separate separate_rows
#' @importFrom dplyr %>% .data filter inner_join mutate
#' select
logstreams_to_data_frame <- function(logData, respId, questionNamesTo) {
  logData <- logData %>%
    pivot_longer(-{{respId}},
                 names_to = questionNamesTo, values_to = "log") %>%
    separate_rows(log, sep = "[|]") %>%
    filter(log != "") %>%
    mutate(log = sub(";$", "", log)) %>%
    separate(log, into = c("timeStamp", "type", "target.tagName", "target.id",
                           "target.class", "which", "metaKey", "pageX", "pageY"),
             sep = ";", fill = "right") %>%
    mutate(broken = as.numeric(is.na(.data$pageY))) %>%
    return()
}
