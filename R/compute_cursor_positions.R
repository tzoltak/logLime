#' @title Computing cursor position at evenly spaced time points
#' @description Function compute the cursor positions at evenly spaced time
#' points to make the returned results useful, for example to draw heat maps.
#' @inheritParams compute_aat
#' @param timeSpan Space between time points in results in milliseconds.
#' @return A data frame with columns:
#' \describe{
#'   \item{respId}{Column(s) defined by \code{respId}.}
#'   \item{screenId}{Column(s) defined by \code{screenId}.}
#'   \item{timeStampRel}{Time point (in milliseconds, counted from the time of
#'                       \emph{pageLoaded} event occurred.)}
#'   \item{pageX}{Last recorded cursor position on horizontal axis before
#'                a given \emph{timeStampRel}.}
#'   \item{pageY}{Last recorded cursor position on vertical axis before
#'                a given \emph{timeStampRel}.}
#'   \item{pageX_rel}{Last recorded \emph{relative} cursor position
#'                    (see \code{\link{compute_relative_positions}}) on
#'                    horizontal axis before a given \emph{timeStampRel}.
#'                    Included only if there was \emph{pageX_rel} column in the
#'                    input data.}
#'   \item{pageY_rel}{Last recorded \emph{relative} cursor position
#'                    (see \code{\link{compute_relative_positions}}) on
#'                    vertical axis before a given \emph{timeStampRel}.
#'                    Included only if there was \emph{pageY_rel} column in the
#'                    input data.}
#' }
#' @seealso \code{\link{compute_relative_positions}},
#' \code{\link{separate_logdata_types}}
#' @importFrom dplyr %>% .data across all_of any_of distinct filter group_by
#' mutate select slice_tail summarise
#' @importFrom tidyr fill
#' @export
compute_cursor_positions <- function(actions,
                                     respId = any_of(c("id", "token", "respid")),
                                     screenId = all_of("screen"),
                                     timeSpan = 250) {
  stopifnot(is.data.frame(actions),
            all(c("type", "timeStampRel", "pageX", "pageY") %in% names(actions)),
            is.numeric(timeSpan), length(timeSpan) == 1,
            !is.na(timeSpan), timeSpan > 0)
  actions <- ungroup(actions)
  respIdColumns <- names(select(actions, {{respId}}))
  screenIdColumns <- names(select(actions, {{screenId}}))

  message("Preprocessing log-data streams.")
  if (!("broken" %in% names(actions))) {
    actions$broken <- 0
  }
  actions <- actions %>%
    select({{respId}}, {{screenId}},
           all_of(c("type", "timeStampRel", "broken", "pageX", "pageY")),
           any_of(c("pageX_rel", "pageY_rel"))) %>%
    group_by(across(c({{respId}}, {{screenId}}))) %>%
    mutate(broken = any(.data$broken != 0),
           brokenTimeStamps = any(.data$timeStampRel < 0),
           lastTimeStampRel = last(.data$timeStampRel)) %>%
    filter(.data$type %in% "mousemove")
  if (any(actions$broken | actions$brokenTimeStamps)) {
    nRemoved <- nrow(actions)
    mRemoved <- actions %>%
      select({{respId}}, {{screenId}}) %>%
      distinct() %>%
      nrow()
    actions <- actions %>%
      filter(!.data$broken, !.data$brokenTimeStamps)
    nRemoved <- nRemoved - nrow(actions)
    mRemoved <- mRemoved - (actions %>%
                              select({{respId}}, {{screenId}}) %>%
                              distinct() %>%
                              nrow())
    message(format(mRemoved, big.mark = "'"),
            " respondent-screens containing broken records (",
            format(nRemoved, big.mark = "'"),
            " records) were removed before computing cursor positions.\n")
  }

  message("Calculating cursor positions.")
  actions %>%
    group_by(across(c({{respId}}, {{screenId}}))) %>%
    summarise(timeStampRel = seq(0, .data$lastTimeStampRel[1], by = timeSpan),
              .groups = "drop") %>%
    left_join(actions %>%
                select({{respId}}, {{screenId}},
                       all_of(c("timeStampRel", "pageX", "pageY")),
                       any_of(c("pageX_rel", "pageY_rel"))) %>%
                mutate(timeStampRel =
                         timeSpan * (.data$timeStampRel %/% timeSpan)) %>%
                group_by(across(c({{respId}}, {{screenId}},
                                  all_of("timeStampRel")))) %>%
                slice_tail(n = 1) %>%
                ungroup(),
              by = c(respIdColumns, screenIdColumns, "timeStampRel")) %>%
    group_by(across(c({{respId}}, {{screenId}}))) %>%
    fill(any_of(c("pageX", "pageY", "pageX_rel", "pageY_rel")),
         .direction = "down") %>%
    ungroup() %>%
    return()
}
