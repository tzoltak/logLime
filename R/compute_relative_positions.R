#' @title Computing relative positions of mousemove events
#' @description Function computes position of *mousemove* events that are
#' expressed relatively to the positions of the most upper-left and bottom-right
#' INPUT elements (used to mark responses) of the survey form. Such recalculated
#' positions are better comparable between respondents (with possible
#' differences in their hardware and software configurations) and are
#' particularly useful for drawing heat-maps or cursor traces.
#' @inheritParams compute_cursor_indices
#' @param systemInfo A data frame containing data regarding
#' *system information* - typically element *systemInfo* of a list
#' returned by [separate_logdata_types].
#' @details Please note that relativization is performed using input positions
#' recorded at the moment of the first entry of a given respondent on a given
#' survey screen, even if there were different *entries* identified in the data.
#' If respondent resized browser window between *entries*, relativization will
#' no longer be valid - use [remove_problems] with argument `level = "screen"`
#' to protect against this issue.
#' @return Input data frame with additional columns:
#' \describe{
#'   \item{pageX_rel}{*Relative* position of an event on horizontal axis.}
#'   \item{pageY_rel}{*Relative* position of an event on vertical axis.}
#'   \item{moveX_rel}{*Relative* distance of a move on horizontal axis.}
#'   \item{moveY_rel}{*Relative* distance of a move on vertical axis.}
#' }
#' Additionally, if *moveXScrollCorrected* and *moveYScrollCorrected*
#' columns are available in the input data, there are included columns:
#' \describe{
#'   \item{moveXScrollCorrected_rel}{*Relative scroll corrected* distance
#'                                   of a move on horizontal axis.}
#'   \item{moveYScrollCorrected_rel}{*Relative scroll corrected* distance
#'                                   of a move on vertical axis.}
#' }
#' @seealso [compute_cursor_indices], [compute_cursor_positions],
#' [separate_logdata_types]
#' @importFrom dplyr %>% .data all_of any_of left_join mutate
#' @export
compute_relative_positions <- function(actions,
                                       systemInfo,
                                       respId = any_of(c("id", "token", "respid")),
                                       screenId = "screen") {
  stopifnot(is.data.frame(actions),
            all(c("pageX", "pageY") %in% names(actions)))
  respIdColumns <- names(select(actions, {{respId}}))
  screenIdColumns <- names(select(actions, {{screenId}}))
  stopifnot(is.data.frame(systemInfo),
            all(respIdColumns %in% names(systemInfo)),
            all(screenIdColumns %in% names(systemInfo)),
            all(c("inputsMinPageX", "inputsMinPageY",
                  "inputsWidth", "inputsHeight") %in% names(systemInfo)),
            all(!(c("inputsMinPageX", "inputsMinPageY",
                    "inputsWidth", "inputsHeight") %in% names(actions))),
            length(intersect(names(actions), names(systemInfo))) > 0)
  if ("entry" %in% names(actions)) {
    message("\nPlease note that the input positions, according to which the relativization will be performed, were recorded only at the moment of a given respondent enetering a given screen for the first time.")
  }
  actions %>%
    left_join(systemInfo %>%
                select({{respId}}, {{screenId}},
                       "inputsMinPageX", "inputsMinPageY",
                       "inputsWidth", "inputsHeight"),
              by = c(respIdColumns, screenIdColumns)) %>%
    mutate(pageX_rel = (.data$pageX - .data$inputsMinPageX) / .data$inputsWidth,
           pageY_rel = (.data$pageY - .data$inputsMinPageY) / .data$inputsHeight,
           across(any_of("moveX"), list(rel = ~. / inputsWidth)),
           across(any_of("moveY"), list(rel = ~. / inputsHeight)),
           across(any_of("moveXScrollCorrected"), list(rel = ~. / inputsWidth)),
           across(any_of("moveYScrollCorrected"), list(rel = ~. / inputsHeight))) %>%
    select(-c("inputsMinPageX", "inputsMinPageY",
              "inputsWidth", "inputsHeight")) %>%
    return()
}
