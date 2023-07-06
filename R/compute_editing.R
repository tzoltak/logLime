#' @title Computing number of (sub)question edits
#' @description Function computes number of edits (i.e. marking or changing an
#' answer) for each (sub)question made by each respondent. Be aware that
#' **returned number of edits includes also first marking of an answer**,
#' so correcting one's response is indicated by number of edits greater than 1.
#' @inheritParams compute_aat
#' @inheritParams compute_cursor_indices
#' @return If `returnFormat` is *long* a data frame with columns:
#' \describe{
#'   \item{respId}{Column(s) defined by `respId`.}
#'   \item{screenId}{Column(s) defined by `screenId`.}
#'   \item{entryId}{Column(s) defined by `entryId` (if any).}
#'   \item{questionCode}{Code of a question.}
#'   \item{subquestionCode}{Code of a subquestion.}
#'   \item{edits}{Number of edits.}
#' }
#'
#' If `returnFormat` is *wide*, returned data frame will not contain columns
#' identifying screen, *entry* (if applies), question and subquestion, but values
#' of these variables will be appended to column names (separated by "_"), with
#' each column reporting the number of edits of a given sub(question) for
#' a given screen (or screen-*entry*).
#' @seealso [separate_logdata_types]
#' @importFrom dplyr %>% .data across all_of any_of count filter mutate select
#' @importFrom tidyr pivot_wider
#' @export
compute_editing <- function(actions,
                            respId = any_of(c("id", "token", "respid")),
                            screenId = "screen",
                            entryId = c(),
                            returnFormat = c("long", "wide")) {
  stopifnot(is.data.frame(actions),
            all(c("questionCode", "subquestionCode") %in% names(actions)))
  actions <- ungroup(actions)
  respIdColumns <- names(select(actions, {{respId}}))
  screenIdColumns <- names(select(actions, {{screenId}}))
  entryIdColumns <- names(select(actions, {{entryId}}))
  returnFormat <- match.arg(returnFormat,
                            c("long", "wide"))

  message("Preprocessing log-data streams.")
  if (!("broken" %in% names(actions))) {
    actions$broken <- 0
  }
  actions <- actions %>%
    group_by(across(c({{respId}}, {{screenId}}, {{entryId}}))) %>%
    mutate(broken = any(.data$broken != 0)) %>%
    filter(.data$type %in% "change")

  if (any(actions$broken)) {
    nRemoved <- nrow(actions)
    mRemoved <- actions %>%
      select(c({{respId}}, {{screenId}}, {{entryId}})) %>%
      distinct() %>%
      nrow()
    actions <- actions %>%
      filter(!.data$broken)
    nRemoved <- nRemoved - nrow(actions)
    mRemoved <- mRemoved - (actions %>%
                              select(c({{respId}}, {{screenId}}, {{entryId}})) %>%
                              distinct() %>%
                              nrow())
    message(format(mRemoved, big.mark = "'"),
            " respondent-screen",
            ifelse(length(entryIdColumns) > 0L, "-entrie", ""),
            "s containing broken records (",
            format(nRemoved, big.mark = "'"),
            " records) were removed before counting edits.\n")
  }

  message("Counting edits.")
  actions <- actions %>%
    ungroup() %>%
    count(across(c({{respId}}, {{screenId}}, {{entryId}},
                   any_of(c("questionCode", "subquestionCode")))),
          name = "edits")
  if (returnFormat == "wide") {
    message("Pivoting results to wide format.")
    actions <- actions %>%
      pivot_wider(id_cols = respIdColumns,
                  names_from = c(screenIdColumns, entryIdColumns,
                                 setdiff(names(actions),
                                         c(respIdColumns, screenIdColumns,
                                           entryIdColumns, "edits"))),
                  names_prefix = "edits_", names_sep = "_",
                  values_from = .data$edits)
  }
  return(actions)
}
