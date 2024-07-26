#' @title Computing indicators describing cursor moves
#' @description Function computes several indices describing the way respondent
#' moves a cursor on a survey screen.
#' @inheritParams compute_aat
#' @param entryId <[tidy-select][dplyr::dplyr_tidy_select]> Variable(s)
#' identifying survey screen *entry* (compare [separate_returns]).
#' Set to `c()` to indicate that results should be returned for the whole survey
#' screens and not separately for each respondent's entry on a given survey
#' screen.
#' @details To get also *relative* versions of indices, call
#' [compute_relative_positions] on the data frame storing actions
#' (events) before passing it into the `actions` argument.
#'
#' Please note that values of average absolute acceleration (and only
#' these indices!) depends on whether *stagnations* were previously
#' separated from *mousemove* events or no - see [separate_stagnations]
#' for details.
#' @return If `returnFormat` is *long*, a data frame with columns:
#' \describe{
#'   \item{respId}{Column(s) defined by `respId`.}
#'   \item{screenId}{Column(s) defined by `screenId`.}
#'   \item{entryId}{Column(s) defined by `entryId` (if any).}
#'   \item{dX}{Total distance traveled on horizontal axis \[px\].}
#'   \item{dY}{Total distance traveled on vertical axis \[px\].}
#'   \item{vX}{Average horizontal speed \[px/s\].}
#'   \item{vY}{Average vertical speed \[px/s\].}
#'   \item{aX}{Average absolute horizontal acceleration \[px/s^2\].}
#'   \item{aY}{Average absolute vertical acceleration \[px/s^2\].}
#'   \item{flipsX}{Number of *flips* (changing direction of move)
#'                 on horizontal axis.}
#'   \item{flipsY}{Number of *flips* (changing direction of move)
#'                 on vertical axis.}
#'   \item{dX_sc, dY_sc, vX_sc, vY_sc, aX_sc, aY_sc, flipsX_sc, flipsY_sc}{
#'         *Scrolling corrected* versions of the aforementioned indices.
#'         i.e. computed ruling out cursor moves over the survey screen that
#'         occurred because of the other actions than moving a pointing device
#'         (in particular: because of scrolling - either using mouse wheel,
#'         touchpad gestures or scrolling bar - using arrows to scroll the page
#'         or using TAB key to switch between survey form INPUT fields).}
#' }
#' Additionally, if *moveX_rel* and *moveY_rel* columns or
#' *moveXScrollCorrected_rel* and *moveYScrollCorrected* columns are
#' available in the input data, there are included columns:
#' \describe{
#'   \item{dX_rel, dY_rel, vX_rel, vY_rel, aX_rel, aY_rel, dX_screl, dY_screl,
#'         vX_screl, vY_screl, aX_screl, aY_screl}{
#'         *Relative* versions of the aforementioned indices, i.e. divided
#'         by the width or height of a rectangle spanned by the most upper-left
#'         and the most bottom-right survey form INPUT element's (used to mark
#'         question answers) position on a given survey screen. These ones are
#'         better comparable between respondents with different browser window
#'         size (see [compute_relative_positions]).}
#' }
#'
#' If `returnFormat` is *wide*, returned data frame will not contain columns
#' identifying screen and *entry* (if applies), but values of these variables
#' will be appended to column names (separated by "_"), with each column
#' reporting the values of a given index for a given screen (or screen-*entry*).
#' @seealso [separate_logdata_types], [compute_relative_positions]
#' @importFrom dplyr %>% .data across all_of any_of filter group_by lag
#' left_join mutate rename_with summarise
#' @importFrom stats weighted.mean
#' @export
compute_cursor_indices <- function(actions,
                                   respId = any_of(c("id", "token", "respid")),
                                   screenId = "screen",
                                   entryId = c(),
                                   returnFormat = c("long", "wide")) {
  stopifnot(is.data.frame(actions),
            all(c("type", "moveX", "moveY", "duration") %in% names(actions)))
  respIdColumns <- names(select(actions, {{respId}}))
  screenIdColumns <- names(select(actions, {{screenId}}))
  entryIdColumns <- names(select(actions, {{entryId}}))
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
    select({{respId}}, {{screenId}}, {{entryId}}, "moveX", "moveY", "duration",
           any_of(c("moveXScrollCorrected", "moveYScrollCorrected",
                    "moveX_rel", "moveY_rel",
                    "moveXScrollCorrected_rel", "moveYScrollCorrected_rel"))) %>%
    group_by(across(c({{respId}}, {{screenId}}, {{entryId}}))) %>%
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
  if (length(entryIdColumns) > 1L &&
      (all(c("moveX_rel", "moveY_rel") %in% names(actions)) ||
       all(c("moveXScrollCorrected_rel",
             "moveYScrollCorrected_rel") %in% names(actions)))) {
    message("Please note that relativization is performed using input positions recorded at the time of a given respondent entering a given screen for the first time.")
  }
  if (all(c("moveX_rel", "moveY_rel") %in% names(actions))) {
    message("Computing relative variants.")
    actions <- actions %>%
      mutate(dX_rel = abs(.data$moveX_rel),
             dY_rel = abs(.data$moveY_rel),
             vX_rel = .data$dX_rel / .data$duration,
             vY_rel = .data$dY_rel / .data$duration,
             aX_rel = abs(.data$vX_rel - lag(.data$vX_rel, default = 0)) / .data$duration,
             aY_rel = abs(.data$vY_rel - lag(.data$vY_rel, default = 0)) / .data$duration)
  }
  if (all(c("moveXScrollCorrected_rel",
            "moveYScrollCorrected_rel") %in% names(actions))) {
    message("Computing relative scrolling-corrected variants.")
    actions <- actions %>%
      mutate(dX_screl = abs(.data$moveXScrollCorrected_rel),
             dY_screl = abs(.data$moveYScrollCorrected_rel),
             vX_screl = .data$dX_screl / .data$duration,
             vY_screl = .data$dY_screl / .data$duration,
             aX_screl = abs(.data$vX_screl - lag(.data$vX_screl, default = 0)) / .data$duration,
             aY_screl = abs(.data$vY_screl - lag(.data$vY_screl, default = 0)) / .data$duration)
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
      pivot_wider(id_cols = respIdColumns,
                  names_from = c(screenIdColumns, entryIdColumns),
                  names_sep = "_",
                  values_from = setdiff(names(actions),
                                        c(respIdColumns, screenIdColumns,
                                          entryIdColumns)))
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
