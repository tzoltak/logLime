#' @title Performing preprocessing of system information
#' @description Function is called internally by [separate_logdata_types] and
#' is not exported (i.e. it is not intended to be called by package users
#' themselves). It selects records describing system information (i.e.
#' respondent's browser and screen) and performs a preprocessing that includes:
#' \itemize{
#'   \item{Renaming columns accordingly to the data they store.}
#'   \item{Converting columns that are supposed to store numeric values to
#'         a numeric type}
#'   \item{Removing duplicates that occurred due to page being reloaded
#'         (typically because of some responses were not given to
#'         *mandatory* questions or some validation criteria defined with
#'         respect to answers were not fulfilled).}
#'   \item{Putting data regarding browser and screen into one row (for each
#'         respondent-screen).}
#'   \item{Including information regarding position of the most upper-left
#'         INPUT element used to mark question answers and width and height of
#'         a rectangle spanned by positions of the most upper-left and the most
#'         bottom-right INPUT elements. This information is useful to perform
#'         standardization of cursor moves indices to correct for
#'         respondent-specific differences in survey layout (see
#'         [compute_cursor_indices]).}
#' }
#' @inheritParams preprocess_actions
#' @inheritParams separate_logdata_types
#' @param inputPositions A data frame with data on INPUT element positions as
#' returned by [preprocess_input_positions].
#' @return A data frame with columns:
#' \describe{
#'   \item{respId}{Column(s) defined by `respId`.}
#'   \item{screenId}{Column(s) defined by `screenId`.}
#'   \item{userAgent}{Value returned by JavaScript navigator.userAgent property.}
#'   \item{language}{Value returned by JavaScript navigator.language property.}
#'   \item{browserWidth}{Width of the browser window.}
#'   \item{browserHeight}{Height of the browser window.}
#'   \item{screenWidth}{Screen width.}
#'   \item{screenHeight}{Screen height.}
#'   \item{inputsMinPageX}{Horizontal position of the most upper-left INPUT
#'                         element on a given survey screen.}
#'   \item{inputsMinPageY}{Vertical position of the most upper-left INPUT
#'                         element on a given survey screen.}
#'   \item{inputsWidth}{Width of a rectangle spanned by positions of the most
#'                      upper-left and the most bottom-right INPUT elements.}
#'   \item{inputsHeight}{Height of a rectangle spanned by positions of the most
#'                       upper-left and the most bottom-right INPUT elements.}
#' }
#' @seealso [find_problems], [separate_logdata_types
#' @importFrom dplyr %>% .data across filter group_by left_join n n_distinct
#' select summarise ungroup
preprocess_system_info <- function(logData, inputPositions, respId, screenId,
                                   inputsBoxCells) {
  logData <- logData %>%
    filter(.data$type %in% c("browser", "screen")) %>%
    select({{respId}}, {{screenId}}, "entry", what = "type",
           userAgent = "target.tagName", language = "target.id",
           width = "pageX", height = "pageY") %>%
    group_by(across(c({{respId}}, {{screenId}}, "entry"))) %>%
    summarise(userAgent = first(.data$userAgent[.data$what %in% "browser"]),
              language = first(.data$language[.data$what %in% "browser"]),
              browserWidth = first(.data$width[.data$what %in% "browser"]),
              browserHeight = first(.data$height[.data$what %in% "browser"]),
              screenWidth = first( .data$width[.data$what %in% "screen"]),
              screenHeight = first(.data$height[.data$what %in% "screen"]),
              .groups = "drop")
  if (!is.null(inputPositions)) {
    if (all(c("questionFormat",
              "subquestionCode", "answerCode") %in% names(inputPositions))) {
      inputsBox <- inputPositions %>%
        group_by(across(c({{respId}}, {{screenId}}))) %>%
        summarise(inputsMinPageX = min(.data$pageX),
                  inputsMinPageY = min(.data$pageY),
                  inputsWidth = max(.data$pageX) - min(.data$pageX),
                  inputsHeight = max(.data$pageY) - min(.data$pageY),
                  nCols = ifelse(all(.data$questionFormat %in% "H"),
                                 n_distinct(.data$subquestionCode),
                                 n_distinct(.data$answerCode)),
                  nRows = ifelse(all(.data$questionFormat %in% "H"),
                                 n_distinct(.data$answerCode),
                                 n_distinct(.data$subquestionCode)),
                  boxType =
                    ifelse(all(.data$questionFormat %in% c("A", "B", "C", "E",
                                                           "F", "H", ";", ";")) &
                             n_distinct(.data$questionId) == 1L,
                           ifelse(inputsBoxCells, "cells", "inputs"),
                           "inputs"),
                  .groups = "drop") %>%
        mutate(inputsMinPageX =
                 ifelse(.data$boxType == "cells",
                        .data$inputsMinPageX -
                          .data$inputsWidth / (.data$nCols - 1) / 2,
                        .data$inputsMinPageX),
               inputsMinPageY =
                 ifelse(.data$boxType == "cells",
                        .data$inputsMinPageY -
                          .data$inputsHeight / (.data$nRows - 1) / 2,
                        .data$inputsMinPageY),
               inputsWidth =
                 ifelse(.data$boxType == "cells",
                        .data$inputsWidth * .data$nCols / (.data$nCols - 1),
                        .data$inputsWidth),
               inputsHeight =
                 ifelse(.data$boxType == "cells",
                        .data$inputsHeight * .data$nRows / (.data$nRows - 1),
                        .data$inputsHeight))
    } else {
      inputsBox <- inputsBox <- inputPositions %>%
        group_by(across(c({{respId}}, {{screenId}}))) %>%
        summarise(inputsMinPageX = min(.data$pageX),
                  inputsMinPageY = min(.data$pageY),
                  inputsWidth = max(.data$pageX) - min(.data$pageX),
                  inputsHeight = max(.data$pageY) - min(.data$pageY),
                  .groups = "drop")
    }
    respScreenIdsColumns <- names(select(logData, {{respId}}, {{screenId}}))
    logData <- logData %>%
      left_join(inputsBox %>%
                  select(-any_of(c("nCols", "nRows"))),
                by = respScreenIdsColumns)
  }
  return(logData)
}
