#' @title Computing hovering indicators
#' @description Function sums up time spent by the cursor over specific elements
#' of the survey screens. If the input data frame contains columns
#' \emph{questionCode}, \emph{subquestionCode} and \emph{answerCode} (or at
#' least some of them) they will be taken into account while defining distinct
#' records of the returned results. Otherwise time will be summarized for each
#' respondent-screen over different values of the \emph{elementType} column.
#' @inheritParams compute_aat
#' @param collapseAnswerCellAndControl A logical value indicating
#' whether in case of table-format questions answer cells and form controls
#' placed inside these cells should be collapsed into one category in the
#' returned data or reported separately. \code{TRUE} by default.
#' @param showPB A logical value indicating whether to show a progress bar.
#' \code{TRUE} by default.
#' @details Please note that you may change the way results are summarized by
#' selecting a specific subset of variables from a data frame you provide as
#' the \code{actions} argument. If you want to compute hovering simply over
#' different types of survey elements, exclude variables \emph{questionCode} and
#' \emph{subquestionCode} from the input data frame.
#' @return A data frame with columns:
#' \describe{
#'   \item{respId}{Column(s) defined by \code{respId}.}
#'   \item{screenId}{Column(s) defined by \code{screenId}.}
#'   \item{questionCode}{Question code (only if it is present in the input
#'                       data).}
#'   \item{subquestionCode}{Subquestion code (only if it is present in the input
#'                          data).}
#'   \item{answerCode}{Answer code (only if it is present in the input data).}
#'   \item{elementType}{Type of the survey interface element.}
#'   \item{hoverTime}{Total time spent by cursor over the element in seconds.}
#' }
#' @seealso \code{\link{separate_logdata_types}}
#' @importFrom dplyr %>% .data across all_of any_of bind_rows cur_data distinct
#' filter group_by last mutate select slice_tail summarise
#' @importFrom utils txtProgressBar getTxtProgressBar setTxtProgressBar
#' @export
compute_hovering <- function(actions,
                             respId = any_of(c("id", "token", "respid")),
                             screenId = all_of("screen"),
                             collapseAnswerCellAndControl = TRUE,
                             showPB = TRUE) {
  stopifnot(is.data.frame(actions),
            all(c("type", "timeStampRel",  "target.id", "target.tagName",
                  "elementType") %in% names(actions)),
            length(collapseAnswerCellAndControl) == 1L,
            collapseAnswerCellAndControl %in% c(TRUE, FALSE),
            length(showPB) == 1L,
            showPB %in% c(TRUE, FALSE))
  actions <- ungroup(actions)
  respIdColumns <- names(select(actions, {{respId}}))
  screenIdColumns <- names(select(actions, {{screenId}}))

  message("Preprocessing log-data streams.")
  if (!("broken" %in% names(actions))) {
    actions$broken <- 0
  }
  actions <- actions %>%
    select({{respId}}, {{screenId}},
           all_of(c("type", "timeStampRel", "broken", "target.id",
                    "target.tagName", "elementType")),
           any_of(c("questionCode", "subquestionCode", "answerCode"))) %>%
    group_by(across(c({{respId}}, {{screenId}}))) %>%
    mutate(broken = any(.data$broken != 0),
           brokenTimeStamps = any(.data$timeStampRel < 0))
  actions <- bind_rows(
    actions %>%
      filter(.data$type %in% c("mouseover", "mouseout")),
    actions %>%
      mutate(timeStampRel = last(.data$timeStampRel)) %>%
      filter(.data$type %in% c("mouseover", "mouseout")) %>%
      slice_tail(n = 1) %>%
      mutate(type = "mouseout")
  )
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
            " records) were removed before computing hovering indices.\n")
  }

  message("Calculating hovering times:")
  if (showPB) {
    pb <- txtProgressBar(0, actions %>%
                           select({{respId}}, {{screenId}}) %>%
                           distinct() %>%
                           nrow(),
                         style = 3)
  } else {
    pb <- NULL
  }
  actions <- actions %>%
    mutate(target = ifelse(is.na(.data$elementType),
                           NA_character_,
                           paste0(ifelse(is.na(.data$target.id),
                                         .data$elementType,
                                         .data$target.id),
                                  .data$target.tagName)),
           elementType = ifelse(is.na(.data$elementType),
                                "other", .data$elementType),
           # a protection against element types not recognized by label_actions()
           across(any_of(c("questionCode", "subquestionCode", "answerCode")),
                  ~ifelse(is.na(target), NA_character_, .))) %>%
    group_by(across(c({{respId}}, {{screenId}}))) %>%
    summarise(compute_hovering_survey_screen(cur_data(), pb),
              .groups = "drop")
  if (!is.null(pb)) {
    close(pb)
  }
  if (nrow(actions) > nrow(distinct(select(actions, -all_of("hoverTime"))))) {
    actions <- actions %>%
      group_by(across(c({{respId}}, {{screenId}},
                        all_of("elementType"),
                        any_of(c("questionCode", "subquestionCode",
                                 "answerCode"))))) %>%
      summarise(hoverTime = sum(.data$hoverTime),
                .groups = "drop")
  }

  if (collapseAnswerCellAndControl) {
    actions <- actions %>%
      group_by(across(-all_of(c("hoverTime", "elementType")))) %>%
      mutate(elementType =
               ifelse(.data$elementType %in% "answer control" &
                        any(.data$elementType %in% "answer cell"),
                      "answer cell", .data$elementType)) %>%
      group_by(across(-all_of("hoverTime"))) %>%
      summarise(hoverTime = sum(.data$hoverTime),
                .groups = "drop")
  }

  actions %>%
    mutate(hoverTime = .data$hoverTime / 1000) %>%
    select({{respId}}, {{screenId}},
           any_of(c("questionCode", "subquestionCode", "answerCode")),
           all_of(c("elementType", "hoverTime"))) %>%
    return()
}
#' @title Computing hovering indicators internals
#' @description Internal function computing hovering times for a given
#' respondent-screen. It provides handling of some possible log-stream
#' inconsistencies (\emph{mousout} not always being preceeded by
#' \emph{mouseover}).
#' @param x A data frame containg only \emph{mouseover} and \emph{mouseout}
#' types of \emph{actions}.
#' @param pb Optionally a handle to the progress bar object.
#' @return A data frame.
#' @noRd
compute_hovering_survey_screen <- function(x, pb = NULL) {
  objects <- x %>%
    select(all_of(c("target", "elementType")),
           any_of(c("questionCode", "subquestionCode", "answerCode"))) %>%
    distinct() %>%
    mutate(hoverTime = 0,
           timeStampTemp = c(0, rep(NA_real_, n() - 1L)))
  lastType <- ""
  for (i in seq_len(nrow(x))) {
    o <- which(objects$target %in% x$target[i])
    if (x$type[i] == "mouseover" && lastType == "mouseover") {
      oLast <- which(objects$target %in% x$target[i - 1L])
      objects$hoverTime[oLast] <- objects$hoverTime[oLast] +
        x$timeStampRel[i] - objects$timeStampTemp[oLast]
      objects$timeStampTemp[oLast] <- NA_real_
    } else if (x$type[i] == "mouseout") {
      if (!is.na(objects$timeStampTemp[o])) {
        objects$hoverTime[o] <- objects$hoverTime[o] +
          x$timeStampRel[i] - objects$timeStampTemp[o]
        objects$timeStampTemp[o] <- NA_real_
      }
    }
    if (x$type[i] == "mouseover") {
      objects$timeStampTemp[o] <-  x$timeStampRel[i]
    }
    lastType <- x$type[i]
  }
  if (!is.null(pb)) {
    setTxtProgressBar(pb, getTxtProgressBar(pb) + 1)
  }
  objects %>%
    select(-.data$target, -.data$timeStampTemp) %>%
    return()
}
