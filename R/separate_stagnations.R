#' @title Separating periods of no move from mousemoves actions
#' @description Functions enables to separate (relatively) long-lasting
#' \emph{mousemove} \emph{actions} into two: one describing a period in which
#' a cursor not moved and another describing actual move.
#' @param actions data frame containg data regarding \emph{actions} - typically
#' element \code{actions} of a list returned by
#' \code{\link{separate_logdata_types}}
#' @param threshold duration in milliseconds - \emph{mousemove} events lasting
#' longer will be separated
#' @details JavaScript \emph{mousemove} event reports cursor position but
#' is triggered by moving a cursor. As a consequence respondent not moving
#' a cursor will be recorded in log-data returned by
#' \code{\link{separate_logdata_types}} as a (relatively) long-lasting (but
#' probably rather \emph{short-distanced}) \emph{mousemove}. This function
#' enables to separate such \emph{actions} into one covering a period of
#' stagnation (not moving a cursor) and another covering actual move. Sadly,
#' this can be done only by dividing \emph{action} on
#' \strong{arbitrarily chosen threshold} of duration - it is reasonable to set
#' it to the value of frequency of collecting \emph{mousemove} events that was
#' used in the LimeSurvey log-data collecting applet (typically 100 ms) or
#' a little bigger value.
#' @return data frame with the same columns as input but (probably) with some
#' additional rows of type \emph{mousemove}
#' @importFrom dplyr %>% .data across all_of any_of arrange bind_rows mutate
#' select
#' @export
separate_stagnations <- function(actions, threshold) {
  stopifnot(is.data.frame(actions),
            all(c("timeStamp", "type", "duration", "moveX", "moveY") %in%
                  names(actions)),
            is.numeric(threshold), length(threshold) == 1, threshold > 0)
  actions <- actions %>%
    mutate(rowNumber = 1:nrow(actions))
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
    select(-all_of("rowNumber")) %>%
    return()
}
