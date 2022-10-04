#' @title Computing relative positions of mousemove events
#' @description Function computes position of \emph{mousemove} events that are
#' expressed relatively to the positions of the most upper-left and bottom-right
#' INPUT elements (used to mark responses) of the survey form. Such recalculated
#' positions are better comparable between respondents (with possible
#' differences in their hardware and software configurations) and are
#' particularly useful for drawing heat-maps or cursor traces.
#' @inheritParams compute_cursor_indices
#' @param systemInfo A data frame containing data regarding
#' \emph{system information} - typically element \code{systemInfo} of a list
#' returned by \code{\link{separate_logdata_types}}.
#' @return Input data frame with additional columns:
#' \describe{
#'   \item{pageX_rel}{\emph{Relative} position of an event on horizontal axis.}
#'   \item{pageY_rel}{\emph{Relative} position of an event on vertical axis.}
#'   \item{moveX_rel}{\emph{Relative} distance of a move on horizontal axis.}
#'   \item{moveY_rel}{\emph{Relative} distance of a move on vertical axis.}
#' }
#' Additionally, if \code{moveXScrollCorrected_} and \emph{moveYScrollCorrected}
#' columns are available in the input data, there are included columns:
#' \describe{
#'   \item{moveXScrollCorrected_rel}{\emph{Relative scroll corrected} distance
#'                                   of a move on horizontal axis.}
#'   \item{moveYScrollCorrected_rel}{\emph{Relative scroll corrected} distance
#'                                   of a move on vertical axis.}
#' }
#' @seealso \code{\link{compute_cursor_indices}},
#' \code{\link{compute_cursor_positions}},
#' \code{\link{separate_logdata_types}}
#' @importFrom dplyr %>% .data all_of any_of left_join mutate
#' @export
compute_relative_positions <- function(actions,
                                       systemInfo,
                                       respId = any_of(c("id", "token", "respid")),
                                       screenId = all_of("screen")) {
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
  actions %>%
    left_join(systemInfo %>%
                select({{respId}}, {{screenId}},
                       all_of(c("inputsMinPageX", "inputsMinPageY",
                                "inputsWidth", "inputsHeight"))),
              by = c(respIdColumns, screenIdColumns)) %>%
    mutate(pageX_rel = (.data$pageX - .data$inputsMinPageX) / .data$inputsWidth,
           pageY_rel = (.data$pageY - .data$inputsMinPageY) / .data$inputsHeight,
           across(any_of("moveX"), list(rel = ~. / inputsWidth)),
           across(any_of("moveY"), list(rel = ~. / inputsHeight)),
           across(any_of("moveXScrollCorrected"), list(rel = ~. / inputsWidth)),
           across(any_of("moveYScrollCorrected"), list(rel = ~. / inputsHeight))) %>%
    select(-all_of(c("inputsMinPageX", "inputsMinPageY",
                     "inputsWidth", "inputsHeight"))) %>%
    return()
}
