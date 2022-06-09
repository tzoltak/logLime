#' @title Computing number of (sub)question edits
#' @description Function computes number of edits (i.e. marking or changing an
#' answer) for each (sub)question made by each respondent. Be aware that
#' \strong{returned number of edits includes also first marking of an answer},
#' so correcting one's response is indicated by number of edits greater than 1.
#' @inheritParams compute_aat
#' @return If \code{returnFormat} is \emph{long} a data frame with columns:
#' \describe{
#'   \item{respId}{Column(s) defined by \code{respId}.}
#'   \item{screenId}{Column(s) defined by \code{screenId}.}
#'   \item{questionCode}{Code of a question.}
#'   \item{subquestionCode}{Code of a subquestion.}
#'   \item{edits}{Number of edits.}
#' }
#' If \code{returnFormat} is \emph{wide} a data frame with columns:
#' \describe{
#'   \item{respId}{Column(s) defined by \code{respId}.}
#'   \item{screenId}{Column(s) defined by \code{screenId}.}
#'   \item{edits_qCode_sqCode}{Columns named in a convention \emph{edits_codes},
#'                             where \emph{qCode} is question code and
#'                             \emph{sqCode} is subquestion code, storing number
#'                             of edits.}
#' }
#' @seealso \code{\link{separate_logdata_types}}
#' @importFrom dplyr %>% .data across all_of any_of count filter mutate select
#' @importFrom tidyr pivot_wider
#' @export
compute_editing <- function(actions,
                            respId = any_of(c("id", "token", "respid")),
                            screenId = all_of("screen"),
                            returnFormat = c("long", "wide")) {
  stopifnot(is.data.frame(actions),
            all(c("questionCode", "subquestionCode") %in% names(actions)))
  actions <- ungroup(actions)
  respIdColumns <- names(select(actions, {{respId}}))
  screenIdColumns <- names(select(actions, {{screenId}}))
  stopifnot(all(respIdColumns %in% names(actions)),
            all(screenIdColumns %in% names(actions)))
  returnFormat <- match.arg(returnFormat,
                            c("long", "wide"))

  message("Preprocessing log-data streams.")
  if (!("broken" %in% names(actions))) {
    actions$broken <- 0
  }
  actions <- actions %>%
    group_by(across(c({{respId}}, {{screenId}}))) %>%
    mutate(broken = any(.data$broken != 0)) %>%
    filter(.data$type %in% "change")

  if (any(actions$broken)) {
    nRemoved <- nrow(actions)
    mRemoved <- actions %>%
      select(c({{respId}}, {{screenId}})) %>%
      distinct() %>%
      nrow()
    actions <- actions %>%
      filter(!.data$broken)
    nRemoved <- nRemoved - nrow(actions)
    mRemoved <- mRemoved - (actions %>%
                              select(c({{respId}}, {{screenId}})) %>%
                              distinct() %>%
                              nrow())
    message(format(mRemoved, big.mark = "'"),
            " respondent-screens containing broken records (",
            format(nRemoved, big.mark = "'"),
            " records) were removed before counting edits.\n")
  }

  message("Counting edits.")
  actions <- actions %>%
    ungroup() %>%
    count(across(c({{respId}}, {{screenId}},
                   any_of(c("questionCode", "subquestionCode")))),
          name = "edits")
  if (returnFormat == "wide") {
    message("Pivoting results to wide format.")
    actions <- actions %>%
      pivot_wider(id_cols = respIdColumns,
                  names_from = c(screenIdColumns,
                                 setdiff(names(actions),
                                         c(respIdColumns, screenIdColumns,
                                           "edits"))),
                  names_prefix = "edits_", names_sep = "_",
                  values_from = .data$edits)
  }
  return(actions)
}
