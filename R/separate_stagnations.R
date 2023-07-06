#' @title Separating periods of no move from mousemoves actions
#' @description Functions enables to separate (relatively) long-lasting
#' *mousemove* *actions* into two distinct *actions*: one describing a period
#' in which a cursor not moved and another describing actual move.
#' @inheritParams compute_aat
#' @param threshold Duration in milliseconds - *mousemove* events lasting
#' longer than a given value will be separated.
#' @details JavaScript *mousemove* event reports cursor position but is
#' triggered by moving a cursor. As a consequence respondent not moving a cursor
#' will be recorded in log-data returned by [separate_logdata_types] as
#' a (relatively) long-lasting (but probably rather *short-distanced*)
#' *mousemove*. This function enables to separate such *actions* into one
#' covering a period of stagnation (not moving a cursor) and another covering
#' actual move. Sadly, this can be done only by dividing *action* on the
#' **arbitrarily chosen threshold** of duration. It is reasonable to set this
#' threshold to the value of frequency of collecting *mousemove* events that was
#' used in the *LimeSurvey* log-data collecting applet (typically 100 ms) or
#' perhaps to a little bigger value.
#'
#' **Be aware that separating *stagnations* affects values of computed
#' afterwards cursor move average absolute accelerations!** With *stagnations*
#' separated, values of these indices are higher. Other cursor indices remain
#' unaffected by whether *stagnations* were separated or not.
#' @return A data frame with the same columns as input but (probably) with some
#' additional rows of type *mousemove*.
#' @seealso [separate_logdata_types], [compute_cursor_indices]
#' @importFrom dplyr %>% .data across all_of any_of arrange bind_rows mutate
#' select
#' @export
separate_stagnations <- function(actions, threshold) {
  stopifnot(is.data.frame(actions),
            all(c("timeStamp", "type", "duration", "moveX", "moveY") %in%
                  names(actions)),
            is.numeric(threshold), length(threshold) == 1, threshold > 0)
  actions <- actions %>%
    mutate(rowNumber = seq_len(nrow(actions)))
  actionsToSeparate <- actions %>%
    filter(.data$type == "mousemove" & .data$duration > threshold)
  bind_rows(actions %>%
              filter(!(.data$rowNumber %in% actionsToSeparate$rowNumber)),
            actionsToSeparate %>%
              mutate(across(any_of(c("timeStamp", "timeStampRel", "duration")),
                            ~. - threshold),
                     across(any_of(c("moveX", "moveY",
                                     "moveXScrollCorrected",
                                     "moveYScrollCorrected")),
                            ~0)),
            actionsToSeparate %>%
              mutate(duration = threshold,
                     rowNumber = .data$rowNumber + 0.5)) %>%
    arrange(.data$rowNumber) %>%
    select(-"rowNumber") %>%
    return()
}
