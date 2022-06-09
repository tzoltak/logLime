#' @title Computing indicators describing cursor moves
#' @description Function computes several indices describing the way respondent
#' moves a cursor on a survey screen.
#' @inheritParams compute_aat
#' @param systemInfo Optionally a data frame containing data regarding
#' \emph{system information} - typically element \code{systemInfo} of a list
#' returned by \code{\link{separate_logdata_types}}. \strong{It is required to
#' compute relative variants of indices.}
#' @details Please note that values of average absolute acceleration (and only
#' these indices!) depends on whether \emph{stagnations} were previously
#' separated from \emph{mousemove} events or no - see
#' \code{\link{separate_stagnations}} for details.
#' @return A data frame with columns:
#' \describe{
#'   \item{respId}{Column(s) defined by \code{respId}.}
#'   \item{screenId}{Column(s) defined by \code{screenId}.}
#'   \item{dX}{Total distance traveled on horizontal axis [px].}
#'   \item{dY}{Total distance traveled on vertical axis [px].}
#'   \item{vX}{Average horizontal speed [px/s].}
#'   \item{vY}{Average vertical speed [px/s].}
#'   \item{aX}{Average absolute horizontal acceleration [px/s^2].}
#'   \item{aY}{Average absolute vertical acceleration[px/s^2].}
#'   \item{flipsX}{Number of \emph{flips} (changing direction of move)
#'                 on horizontal axis.}
#'   \item{flipsY}{Number of \emph{flips} (changing direction of move)
#'                 on vertical axis.}
#'   \item{dX_sc, dY_sc, vX_sc, vY_sc, aX_sc, aY_sc, flipsX_sc, flipsY_sc}{
#'         \emph{Scrolling corrected} versions of the aforementioned indices.
#'         i.e. computed ruling out cursor moves over the survey screen that
#'         occurred because of the other actions than moving a pointing device
#'         (in particular: because of scrolling - either using mouse wheel,
#'         touchpad gestures or scrolling bar - using arrows to scroll the page
#'         or using TAB key to switch between survey form INPUT fields).}
#' }
#' Additionally, if \code{systemInfo} argument was provided there are included
#' columns:
#' \describe{
#'   \item{dX_rel, dY_rel, vX_rel, vY_rel, aX_rel, aY_rel, dX_screl, dY_screl,
#'         vX_screl, vY_screl, aX_screl, aY_screl}{
#'         \emph{Relative} versions of the aforementioned indices, i.e. divided
#'         by the width or height of a rectangle spanned by the most upper-left
#'         and the most bottom-right survey form INPUT element's (used to mark
#'         question answers) position on a given survey screen. These ones are
#'         better comparable between respondents with different browser window
#'         size.}
#' }
#' @seealso \code{\link{separate_logdata_types}}
#' @importFrom dplyr %>% .data across all_of any_of filter group_by lag mutate
#' rename_with summarise
#' @importFrom stats weighted.mean
#' @export
compute_cursor_indices <- function(actions,
                                   respId = any_of(c("id", "token", "respid")),
                                   screenId = all_of("screen"),
                                   returnFormat = c("long", "wide"),
                                   systemInfo = NULL) {
  stopifnot(is.data.frame(actions),
            all(c("type", "moveX", "moveY", "duration") %in% names(actions)))
  respIdColumns <- names(select(actions, {{respId}}))
  screenIdColumns <- names(select(actions, {{screenId}}))
  returnFormat <- match.arg(returnFormat,
                            c("long", "wide"))
  if ("broken" %in% names(actions)) {
    nRemoved <- nrow(actions)
    actions <- actions %>%
      filter(.data$broken != 1)
    nRemoved <- nRemoved - nrow(actions)
    if (nRemoved > 0L) {
      message(format(nRemoved, big.mark = "'"),
              " broken records were removed before computing cursor moves indices.\n",
              "Be aware that indices are computed also for respondent-screens that contained these broken records (after excluding them).\n")
    }
  }
  message("Computing indices for individual moves.")
  actions <- actions %>%
    filter(.data$type == "mousemove") %>%
    select({{respId}}, {{screenId}},
           all_of(c("moveX", "moveY", "duration")),
           any_of(c("moveXScrollCorrected", "moveYScrollCorrected"))) %>%
    group_by(across(c({{respId}}, {{screenId}}))) %>%
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
    message("Computing scrolling-corrected versions.")
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
    message("Computing relative variants.")
    stopifnot(is.data.frame(systemInfo),
              all(respIdColumns %in% names(systemInfo)),
              all(screenIdColumns %in% names(systemInfo)),
              all(c("inputsWidth", "inputsHeight") %in% names(systemInfo)),
              length(intersect(names(actions), names(systemInfo))) > 0)
    actions <- left_join(actions,
                         systemInfo %>%
                           select({{respId}}, {{screenId}},
                                  all_of(c("inputsWidth", "inputsHeight"))),
                         by = c(respIdColumns, screenIdColumns)) %>%
      mutate(across(any_of(c("dX", "vX", "aX", "dX_sc", "vX_sc", "aX_sc")),
                    list(rel = ~./inputsWidth)),
             across(any_of(c("dY", "vY", "aY", "dY_sc", "vY_sc", "aY_sc")),
                    list(rel = ~./inputsHeight))) %>%
      rename_with(~sub("_sc_rel$", "_screl", .))
  }
  message("Aggregating indices.")
  actions <- actions %>%
    filter(!is.na(.data$duration)) %>%
    summarise(across(any_of(c("dX", "dX_sc", "dX_rel", "dX_screl",
                              "dY", "dY_sc", "dY_rel", "dY_screl",
                              "flipsX", "flipsX_sc", "flipsY", "flipsY_sc")),
                     sum, na.rm = TRUE),
              across(any_of(c("vX", "aX", "vX_sc", "aX_sc",
                              "vY", "aY", "vY_sc", "aY_sc",
                              "vX_rel", "aX_rel", "vX_screl", "aX_screl",
                              "vY_rel", "aY_rel", "vY_screl", "aY_screl")),
                     weighted.mean, w = .data$duration, na.rm = TRUE),
              .groups = "drop")

  if (returnFormat == "wide") {
    message("Pivoting results to wide format.")
    actions <- actions %>%
      pivot_wider(id_cols = respIdColumns, names_from = screenIdColumns,
                  names_sep = "_",
                  values_from = setdiff(names(actions), c(respIdColumns,
                                                          screenIdColumns)))
  }
  return(actions)
}
#' @title Auqiliary data transformations
#' @description Function substitutes zeros in a vector by the last non-zero
#' value preceeding a given one.
#' x A numeric vector.
#' @return A numeric vector.
#' @noRd
change_to_last_not0 <- function(x) {
  if (x[1L] %in% 0) x[1L] <- NA_real_
  if (length(x) == 1L) return(x)
  for (i in seq_along(x)[-1L]) {
    if (x[i] %in% 0) x[i] <- x[i - 1L]
  }
  return(x)
}
