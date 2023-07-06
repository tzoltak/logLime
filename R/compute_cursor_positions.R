#' @title Computing cursor position at evenly spaced time points
#' @description Function compute the cursor positions at evenly spaced time
#' points to make the returned results useful, for example to draw heat maps.
#' @inheritParams compute_cursor_indices
#' @param timeSpan Space between time points in results in milliseconds.
#' @return A data frame with columns:
#' \describe{
#'   \item{respId}{Column(s) defined by `respId`.}
#'   \item{screenId}{Column(s) defined by `screenId`.}
#'   \item{entryId}{Column(s) defined by `entryId` (if any).}
#'   \item{timeStampRel}{Time point (in milliseconds, counted from the time of
#'                       *pageLoaded* event occurred.)}
#'   \item{pageX}{Last recorded cursor position on horizontal axis before
#'                a given *timeStampRel*.}
#'   \item{pageY}{Last recorded cursor position on vertical axis before
#'                a given *timeStampRel*.}
#'   \item{pageX_rel}{Last recorded *relative* cursor position
#'                    (see [compute_relative_positions]) on
#'                    horizontal axis before a given *timeStampRel*.
#'                    Included only if there was *pageX_rel* column in the
#'                    input data.}
#'   \item{pageY_rel}{Last recorded *relative* cursor position
#'                    (see [compute_relative_positions]) on
#'                    vertical axis before a given *timeStampRel*.
#'                    Included only if there was *pageY_rel* column in the
#'                    input data.}
#' }
#' @seealso [compute_relative_positions], [separate_logdata_types]
#' @importFrom dplyr %>% .data across all_of any_of distinct filter first
#' group_by last mutate reframe select slice_tail
#' @importFrom tidyr fill
#' @export
compute_cursor_positions <- function(actions,
                                     respId = any_of(c("id", "token", "respid")),
                                     screenId = "screen",
                                     entryId = any_of("entry"),
                                     timeSpan = 250) {
  stopifnot(is.data.frame(actions),
            all(c("type", "timeStampRel", "pageX", "pageY") %in% names(actions)),
            is.numeric(timeSpan), length(timeSpan) == 1,
            !is.na(timeSpan), timeSpan > 0)
  actions <- ungroup(actions)
  respIdColumns <- names(select(actions, {{respId}}))
  screenIdColumns <- names(select(actions, {{screenId}}))
  entryIdColumns <- names(select(actions, {{entryId}}))

  message("Preprocessing log-data streams.")
  if (!("broken" %in% names(actions))) {
    actions$broken <- 0
  }
  actions <- actions %>%
    select({{respId}}, {{screenId}}, {{entryId}},
           all_of(c("type", "timeStampRel", "broken", "pageX", "pageY")),
           any_of(c("pageX_rel", "pageY_rel"))) %>%
    group_by(across(c({{respId}}, {{screenId}}, {{entryId}}))) %>%
    mutate(broken = any(.data$broken != 0),
           brokenTimeStamps = any(.data$timeStampRel < 0),
           firstTimeStampRel = first(.data$timeStampRel),
           lastTimeStampRel = last(.data$timeStampRel)) %>%
    filter(.data$type %in% "mousemove")
  if (any(actions$broken | actions$brokenTimeStamps)) {
    nRemoved <- nrow(actions)
    mRemoved <- actions %>%
      select({{respId}}, {{screenId}}, {{entryId}}) %>%
      distinct() %>%
      nrow()
    actions <- actions %>%
      filter(!.data$broken, !.data$brokenTimeStamps)
    nRemoved <- nRemoved - nrow(actions)
    mRemoved <- mRemoved - (actions %>%
                              select({{respId}}, {{screenId}}, {{entryId}}) %>%
                              distinct() %>%
                              nrow())
    message(format(mRemoved, big.mark = "'"),
            " respondent-screen",
            ifelse(length(entryIdColumns) > 0L, "-entrie", ""),
            "s containing broken records (",
            format(nRemoved, big.mark = "'"),
            " records) were removed before computing cursor positions.\n")
  }
  if (length(entryIdColumns) > 0L) {
    actions <- actions %>%
      mutate(timeStampRel = .data$timeStampRel - .data$firstTimeStampRel,
             lastTimeStampRel = .data$lastTimeStampRel - .data$firstTimeStampRel)
  }

  message("Calculating cursor positions.")
  actions %>%
    group_by(across(c({{respId}}, {{screenId}}, {{entryId}}))) %>%
    reframe(timeStampRel = seq(0, .data$lastTimeStampRel[1], by = timeSpan)) %>%
    left_join(actions %>%
                select({{respId}}, {{screenId}}, {{entryId}},
                       "timeStampRel", "pageX", "pageY",
                       any_of(c("pageX_rel", "pageY_rel"))) %>%
                mutate(timeStampRel =
                         timeSpan * (.data$timeStampRel %/% timeSpan)) %>%
                group_by(across(c({{respId}}, {{screenId}}, {{entryId}},
                                  "timeStampRel"))) %>%
                slice_tail(n = 1) %>%
                ungroup(),
              by = c(respIdColumns, screenIdColumns, entryIdColumns,
                     "timeStampRel")) %>%
    group_by(across(c({{respId}}, {{screenId}}, {{entryId}}))) %>%
    fill(any_of(c("pageX", "pageY", "pageX_rel", "pageY_rel")),
         .direction = "down") %>%
    ungroup() %>%
    return()
}
