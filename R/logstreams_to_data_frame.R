#' @title Transforming a data frame with log-data streams to a data frame in
#' a long format
#' @description Function is called internally by[separate_logdata_types] and
#' is not exported (i.e. it is not intended to be called by package users
#' themselves). It pivots columns of a data frame storing log-data streams
#' to the *long* format and then separates log-data streams into different rows
#' and columns. It performs **no** log-data consistency checks or additional
#' transformations (like assuring formats of specific columns) - this is done
#' only afterwards by other functions (see [preprocess_actions],
#' [preprocess_system_info] and [preprocess_input_positions]).
#' @inheritParams separate_logdata_types
#' @return A data frame with columns:
#' \describe{
#'   \item{timeStamp}{Time stamp of an event.}
#'   \item{type}{Type of event.}
#'   \item{target.tagName}{Type of a HTML element that triggered an event (may
#'                         be empty for some types of events).}
#'   \item{target.id}{Id of a HTML element that triggered an event (may be empty
#'                    for some types of events).}
#'   \item{target.class}{CSS classes assigned to a HTML element that triggered
#'                       an event (may be empty for some types of events).}
#'   \item{which}{Information about which mouse button was pressed.}
#'   \item{metaKey}{Information regrading a meta key (ALT, CTRL) was pressed.}
#'   \item{pageX}{Location of an event on the webpage - horizontal axis.}
#'   \item{pageY}{Location of an event on the webpage - vertical axis.}
#'   \item{broken}{Flag indicating whether a record is broken.}
#' }
#' @seealso [separate_logdata_types], [check_for_semicolons_in_strings]
#' @importFrom tidyr pivot_longer separate separate_rows
#' @importFrom dplyr %>% .data across filter inner_join mutate select
logstreams_to_data_frame <- function(logData, respId, questionNamesTo,
                                     questionNamesPrefix) {
  respIdColumns <- names(select(logData, {{respId}}))
  names(logData)[!(names(logData) %in% respIdColumns)] <-
    sub(paste0("^", questionNamesPrefix), "",
        names(logData)[!(names(logData) %in% respIdColumns)])
  message("  Pivoting data with log-streams in many columns to put it into one column...")
  logData <- pivot_longer(logData, -{{respId}},
                          names_to = questionNamesTo, values_to = "log")
  message("  Separating log-streams into rows...")
  logData <- logData %>%
    separate_rows(log, sep = "[|]") %>%
    filter(log != "") %>%
    mutate(log = sub(";$", "", log))
  message("  Separating log-streams into columns...")
  logData <- logData %>%
    check_for_semicolons_in_strings() %>%
    separate(log, into = c("timeStamp", "type", "target.tagName", "target.id",
                           "target.class", "which", "metaKey", "pageX", "pageY"),
             sep = ";", fill = "right")
  message("  Marking broken records...")
  logData <- logData %>%
    mutate(broken = as.numeric(is.na(.data$pageY)))
  message("Done.")
  return(logData)
}
