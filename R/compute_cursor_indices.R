#' @title Computing indicators describing cursor moves
#' @description Function computes few indices describing the way respondent
#' moves a cursor on a survey screen.
#' @param actions data frame containg data regarding \emph{actions} - typically
#' element \code{actions} of a list returned by
#' \code{\link{separate_logdata_types}}
#' @param respScreenIds <tidy-select> names of columns identifying
#' respondent-screens
#' @param systemInfo optionally data frame containg data regarding
#'  \emph{system information} - typically element \code{systemInfo} of a list
#'  returned by \code{\link{separate_logdata_types}}; it is required to compute
#'  relative variants of indices
#' @return data frame with columns:
#' \itemize{
#'   \item{Columns defined by \code{respScreenIds}}
#'   \item{\code{dX} - total distance travelled on horizontal axis,}
#'   \item{\code{dY} - total dostance travelled on vertical axis,}
#'   \item{\code{vX} - average horizontal speed,}
#'   \item{\code{vY} - average vertical speed,}
#'   \item{\code{aX} - average absolute horizontal acceleration,}
#'   \item{\code{aY} - average absolute vertical acceleration,}
#'   \item{\code{flipsX} - number of \emph{flips} (changing direction of move) on horizontal axis,}
#'   \item{\code{flipsY} - number of \emph{flips} (changing direction of move) on vertical axis,}
#'   \item{Aforementioned columns with suffix \emph{_sc} - analogous indiced
#'         computed using cursor moves corrected for scrolling (i.e. not taking
#'         into acount cursor moves on a survey screen that took place because
#'         of other actions than moving a cursor itself using a pointing device).}
#' }
#' and if \code{systemInfo} argument was provided with additional columns:
#' \itemize{
#'   \item{Aforementioned columns, with the exception of \code{flipsX} and
#'         \code{flipsY}, with suffix \emph{_rel} or \emph{_screl} - relative
#'         variants of the aforementioned indices, i.e. divided by the width or
#'         height of a rectangle spanned by the most upper-left and the most
#'         bottom-right INPUT element's - used to mark question answers -
#'         position on a given survey screen.}
#' }
#' @seealso \code{\link{separate_logdata_types}}
#' \code{\link{preprocess_system_info}}
#' \code{\link{preprocess_input_positions}}
#' @importFrom dplyr %>% .data across all_of any_of filter group_by lag mutate
#' rename_with summarise
#' @importFrom stats weighted.mean
#' @export
compute_cursor_indices <- function(actions,
                                   respScreenIds =
                                     all_of(intersect(names(actions),
                                                      c("id", "token", "screen"))),
                                   systemInfo = NULL) {
  stopifnot(is.data.frame(actions),
            all(c("type", "moveX", "moveY", "duration") %in% names(actions)))
  respScreenIdsColumns <- names(select(actions, {{respScreenIds}}))
  if ("broken" %in% names(actions)) {
    actions <- actions %>%
      filter(.data$broken != 1)
  }
  actions <- actions %>%
    filter(.data$type == "mousemove") %>%
    select(all_of(c(respScreenIdsColumns, "moveX", "moveY", "duration")),
           any_of(c("moveXScrollCorrected", "moveYScrollCorrected"))) %>%
    group_by(across(all_of(respScreenIdsColumns))) %>%
    mutate(dX = abs(.data$moveX),
           dY = abs(.data$moveY),
           vX = .data$dX / .data$duration,
           vY = .data$dY / .data$duration,
           aX = abs(.data$vX - lag(.data$vX, default = 0)) / .data$duration,
           aY = abs(.data$vY - lag(.data$vY, default = 0)) / .data$duration,
           dirX = change_to_last_not0(sign(.data$moveX)),
           dirY = change_to_last_not0(sign(.data$moveY)),
           flipsX = as.integer(.data$dirX != lag(.data$dirX)),
           flipsY = as.integer(.data$dirY != lag(.data$dirY)))
  if (all(c("moveXScrollCorrected",
            "moveYScrollCorrected") %in% names(actions))) {
    actions <- actions %>%
      mutate(dX_sc = abs(.data$moveXScrollCorrected),
             dY_sc = abs(.data$moveYScrollCorrected),
             vX_sc = .data$dX_sc / .data$duration,
             vY_sc = .data$dY_sc / .data$duration,
             aX_sc = abs(.data$vX_sc - lag(.data$vX_sc, default = 0)) / .data$duration,
             aY_sc = abs(.data$vY_sc - lag(.data$vY_sc, default = 0)) / .data$duration,
             dirX_sc = change_to_last_not0(sign(.data$moveXScrollCorrected)),
             dirY_sc = change_to_last_not0(sign(.data$moveYScrollCorrected)),
             flipsX_sc = as.integer(.data$dirX_sc != lag(.data$dirX_sc)),
             flipsY_sc = as.integer(.data$dirY_sc != lag(.data$dirY_sc)))
  }
  if (!is.null(systemInfo)) {
    stopifnot(is.data.frame(systemInfo),
              all(respScreenIdsColumns %in% names(systemInfo)),
              all(c("inputsWidth", "inputsHeight") %in% names(systemInfo)),
              length(intersect(names(actions), names(systemInfo))) > 0)
    actions <- left_join(actions,
                         systemInfo %>%
                           select(all_of(c(respScreenIdsColumns,
                                           "inputsWidth", "inputsHeight"))),
                         by = respScreenIdsColumns) %>%
      mutate(across(any_of(c("dX", "vX", "aX", "dX_sc", "vX_sc", "aX_sc")),
                    list(rel = ~./inputsWidth)),
             across(any_of(c("dY", "vY", "aY", "dY_sc", "vY_sc", "aY_sc")),
                    list(rel = ~./inputsHeight))) %>%
      rename_with(~sub("_sc_rel$", "_screl", .))
  }
  actions %>%
    filter(!is.na(.data$duration)) %>%
    summarise(across(any_of(c("dX", "dX_sc", "dX_rel", "dX_screl",
                              "dY", "dY_sc", "dY_rel", "dY_screl",
                              "flipsX", "flipsX_sc", "flipsy", "flipsY_sc")),
                     sum, na.rm = TRUE),
              across(any_of(c("vX", "aX", "vX_sc", "aX_sc",
                              "vY", "aY", "vY_sc", "aY_sc",
                              "vX_rel", "aX_rel", "vX_screl", "aX_screl",
                              "vY_rel", "aY_rel", "vY_screl", "aY_screl")),
                     weighted.mean, w = .data$duration, na.rm = TRUE),
              .groups = "drop") %>%
    return()
}
change_to_last_not0 <- function(x) {
  if (x[1L] %in% 0) x[1L] <- NA_real_
  if (length(x) == 1L) return(x)
  for (i in seq_along(x)[-1L]) {
    if (x[i] %in% 0) x[i] <- x[i - 1L]
  }
  return(x)
}
