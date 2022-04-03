#' @title Performing preprocessing of system information
#' @description Function is called internally by
#' \code{\link{separate_logdata_types}} and is not exported (i.e. it is not
#' intended to be called by package users themselves). It selects records
#' describing system information (i.e. respondent's browser and screen) and
#' performs a preprocessing that includes:
#' \itemize{
#'   \item{Renaming columns accordingly to the data they store.}
#'   \item{Converting column types storing numeric values to numeric type.}
#'   \item{Removing duplicates that occurred due to page being reloaded
#'         (typically because of some responses were not given to
#'         \emph{mandatory} questions or some criteria defined with respect to
#'         answers were not fulfilled).}
#'   \item{Putting data regarding browser and screen into one row (for each
#'         respondent-screen).}
#'   \item{Including information regarding position of the most upper-left
#'         INPUT element used to mark question answers and width and height of
#'         a rectangle spanned by the most upper-left and the most bottom-right
#'         INPUT element's - used to mark question answers -  position.
#'         This information is useful to perform standardization of cursor
#'         moves indices to correct for respondent-specific differences in
#'         survey layout.}
#' }
#' @param logData data frame with log-data as returned by
#' \code{\link{logstreams_to_data_frame}}
#' @param inputPositions data frame with data on INPUT element positions as
#' returned by \code{\link{preprocess_input_positions}}
#' @param respScreenIds <tidy-select> names of columns identifying
#' respondent-screens
#' @return data frame with columns:
#' \itemize{
#'   \item{Columns defined by \code{respScreenIds}}
#'   \item{userAgent,}
#'   \item{language,}
#'   \item{browserWidth,}
#'   \item{browserHeight,}
#'   \item{screenWidth,}
#'   \item{screenHeight,}
#'   \item{inputsMinPageX,}
#'   \item{inputsMinPageY,}
#'   \item{inputsWidth.}
#'   \item{inputsHeight.}
#' }
#' @seealso \code{\link{find_problems}}
#' @importFrom dplyr %>% .data across filter group_by left_join select summarise
#' ungroup
preprocess_system_info <- function(logData, inputPositions, respScreenIds) {
  logData %>%
    filter(.data$type %in% c("browser", "screen")) %>%
    select({{respScreenIds}}, what = .data$type,
           userAgent = .data$target.tagName, language = .data$target.id,
           width = .data$pageX, height = .data$pageY) %>%
    group_by(across({{respScreenIds}})) %>%
    summarise(userAgent = first(.data$userAgent[.data$what %in% "browser"]),
              language = first(.data$language[.data$what %in% "browser"]),
              browserWidth = first(.data$width[.data$what %in% "browser"]),
              browserHeight = first(.data$height[.data$what %in% "browser"]),
              screenWidth = first( .data$width[.data$what %in% "screen"]),
              screenHeight = first(.data$height[.data$what %in% "screen"]),
              .groups = "drop") %>%
    ungroup() %>%
    left_join(inputPositions %>%
                group_by(across({{respScreenIds}})) %>%
                summarise(inputsMinPageX = min(.data$pageX),
                          inputsMinPageY = min(.data$pageY),
                          inputsWidth = max(.data$pageX) - min(.data$pageX),
                          inputsHeight = max(.data$pageY) - min(.data$pageY)),
              by = {{respScreenIds}}) %>%
    return()
}
