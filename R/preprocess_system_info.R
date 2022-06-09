#' @title Performing preprocessing of system information
#' @description Function is called internally by
#' \code{\link{separate_logdata_types}} and is not exported (i.e. it is not
#' intended to be called by package users themselves). It selects records
#' describing system information (i.e. respondent's browser and screen) and
#' performs a preprocessing that includes:
#' \itemize{
#'   \item{Renaming columns accordingly to the data they store.}
#'   \item{Converting columns that are supposed to store numeric values to
#'         a numeric type}
#'   \item{Removing duplicates that occurred due to page being reloaded
#'         (typically because of some responses were not given to
#'         \emph{mandatory} questions or some validation criteria defined with
#'         respect to answers were not fulfilled).}
#'   \item{Putting data regarding browser and screen into one row (for each
#'         respondent-screen).}
#'   \item{Including information regarding position of the most upper-left
#'         INPUT element used to mark question answers and width and height of
#'         a rectangle spanned by positions of the most upper-left and the most
#'         bottom-right INPUT elements. This information is useful to perform
#'         standardization of cursor moves indices to correct for
#'         respondent-specific differences in survey layout (see
#'         \code{\link{compute_cursor_indices}}).}
#' }
#' @inheritParams preprocess_actions
#' @param inputPositions A data frame with data on INPUT element positions as
#' returned by \code{\link{preprocess_input_positions}}.
#' @return A data frame with columns:
#' \describe{
#'   \item{respId}{Column(s) defined by \code{respId}.}
#'   \item{screenId}{Column(s) defined by \code{screenId}.}
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
#' @seealso \code{\link{find_problems}} \code{\link{separate_logdata_types}}
#' @importFrom dplyr %>% .data across filter group_by left_join select summarise
#' ungroup
preprocess_system_info <- function(logData, inputPositions, respId, screenId) {
  respScreenIdsColumns <- names(select(logData, {{respId}}, {{screenId}}))
  logData %>%
    filter(.data$type %in% c("browser", "screen")) %>%
    select({{respId}}, {{screenId}}, what = .data$type,
           userAgent = .data$target.tagName, language = .data$target.id,
           width = .data$pageX, height = .data$pageY) %>%
    group_by(across(c({{respId}}, {{screenId}}))) %>%
    summarise(userAgent = first(.data$userAgent[.data$what %in% "browser"]),
              language = first(.data$language[.data$what %in% "browser"]),
              browserWidth = first(.data$width[.data$what %in% "browser"]),
              browserHeight = first(.data$height[.data$what %in% "browser"]),
              screenWidth = first( .data$width[.data$what %in% "screen"]),
              screenHeight = first(.data$height[.data$what %in% "screen"]),
              .groups = "drop") %>%
    ungroup() %>%
    left_join(inputPositions %>%
                group_by(across(c({{respId}}, {{screenId}}))) %>%
                summarise(inputsMinPageX = min(.data$pageX),
                          inputsMinPageY = min(.data$pageY),
                          inputsWidth = max(.data$pageX) - min(.data$pageX),
                          inputsHeight = max(.data$pageY) - min(.data$pageY)),
              by = respScreenIdsColumns) %>%
    return()
}
