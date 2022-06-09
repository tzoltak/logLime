#' @title Computing answering time indicators
#' @description Functions enables to compute several indicators describing
#' respondent's answering speed using either data on time of changing
#' states of the survey form controls (\code{compute_aat}) or previously
#' computed (using \code{\link{compute_hovering}}) hovering times.
#' @param actions A data frame containing data regarding \emph{actions} -
#' typically element \emph{actions} of a list returned by
#' \code{\link{separate_logdata_types}}.
#' @param respId <\code{\link[dplyr:dplyr_tidy_select]{tidy-select}}>
#' Variable(s) identifying respondent.
#' @param screenId <\code{\link[dplyr:dplyr_tidy_select]{tidy-select}}>
#' Variable(s) identifying survey screen.
#' @param returnFormat String indicating whether results should be returned in
#' a \emph{long} (row is respondent-(sub)question) or in a \emph{wide}
#' (row is a respondent, there is a separate column for each (sub)question)
#' format. Can be abbreviated.
#' @param numberOfItems Optionally a data frame containing: survey screen
#' identifier(s) (same as provided by \code{screenId}) and a column
#' \emph{nItems} specifying number of items on a given screen. The latter will
#' be used as denominator while computing average answering time indicators in
#' their variants labeled \emph{All} (see section on returned results) instead
#' of determining these numbers given data provided by the \code{actions} (or
#' \code{hoverTimes}) argument. One may use function
#' \code{\link{count_number_of_items}} to prepare such a data frame.
#' @param multipleQuestionsAction String indicating what to do with
#' respondent-screens containing more than 1 question - default value
#' (\emph{exclude}) means returning \code{NAs} in average answering time
#' indicators. That is because these measures were developed with respect to the
#' questions in a tabular form presented in a one-question-per-survey screen
#' manner. Nevertheless one may change this argument to \emph{keep} to get
#' indices computed also for screens containing many questions (in such a case
#' for purpose of counting the number of \emph{items} a question with no
#' subquestions defined is treated as a single \emph{item}).
#' @details Functions discard survey-screens for which there are some broken
#' records.
#'
#' Set of indicators that is returned depends on whether input data
#' contains variables \emph{questionCode} and \emph{subquestionCode}, because
#' without these two it can not be determined to which subquestion a given
#' \emph{action} relates to. See section on returned results below.
#' @return If \code{returnFormat} is \emph{long} a data frame with columns:
#' \describe{
#'   \item{respId}{Column(s) defined by \code{respId}.}
#'   \item{screenId}{Column(s) defined by \code{screenId}.}
#'   \item{timeToFirstAnswer}{Time to first answer (change of survey form
#'                            control for some item).
#'                            \strong{Returned only by \code{compute_aat}.}}
#'   \item{averageAnsweringTime}{Average time spent on answering a single item
#'                               (estimated excluding first item that was
#'                               answered on the survey screen), \strong{taking
#'                               into account only items that were answered}.
#'                               \strong{Returned only by \code{compute_aat}.}
#'                               Not returned if the input data do not contain
#'                               columns \emph{questionCode} or
#'                               \code{subquestionCode}.}
#'   \item{pageTimeIndex}{Screen time (\code{totalTime}) divided by the number
#'                        of items \strong{that were answered} on a given survey
#'                        screen. \strong{Returned only by \code{compute_aat}.}
#'                        Not returned if the input data do not contain columns
#'                        \emph{questionCode} or \code{subquestionCode}.}
#'   \item{averageAnsweringTimeAll}{Average time spent on answering a single
#'                                  item (estimated excluding first item that
#'                                  was answered on the survey screen), but
#'                                  computed by \strong{dividing by the number
#'                                  of all the items on a given survey screen
#'                                  (irrespective they were answered or
#'                                  omitted)}. \strong{Returned only by
#'                                  \code{compute_aat}.}}
#'   \item{pageTimeIndexAll}{Screen time (\code{totalTime}) \strong{divided by
#'                           the number of all the items on a given survey
#'                           screen (irrespective they were answered or
#'                           omitted)}.}
#'   \item{totalTime}{Screen time, i.e. total time spent on the survey screen.
#'                    \strong{Returned only by \code{compute_aht}.}}
#'   \item{averageHoverTimeAll}{Average time of mouse cursor hovering over
#'                              a single item (taking into account both content
#'                              and answers), taking into account \strong{all
#'                              the items on a given survey screen (irrespective
#'                              they were answered or omitted)}.
#'                               \strong{Returned only by \code{compute_aht}.}}
#' }
#' If \code{returnFormat} is \emph{wide}: a data frame with columns:
#' \describe{
#'   \item{respId}{Column(s) defined by \code{respId}.}
#'   \item{index_screen}{Columns named in a convention \emph{index_screen} where
#'                       \emph{index} is one of the names listed above with
#'                       respect to the \emph{long} format of returned results
#'                       and \emph{screen} is value of the column(s) defined by
#'                        \code{screenId}.}
#' }
#' @seealso \code{\link{separate_logdata_types}}
#' @importFrom dplyr %>% .data across all_of any_of bind_rows distinct filter
#' group_by left_join mutate n_distinct rename select
#' slice slice_tail summarise starts_with ungroup
#' @importFrom tidyr pivot_wider
#' @export
compute_aat <- function(actions,
                        respId = any_of(c("id", "token", "respid")),
                        screenId = all_of("screen"),
                        returnFormat = c("long", "wide"),
                        numberOfItems = NULL,
                        multipleQuestionsAction = c("exclude", "keep")) {
  stopifnot(is.data.frame(actions),
            all(c("type", "timeStampRel") %in% names(actions)))
  actions <- ungroup(actions)
  respIdColumns <- names(select(actions, {{respId}}))
  screenIdColumns <- names(select(actions, {{screenId}}))
  returnFormat <- match.arg(returnFormat,
                            c("long", "wide"))
  multipleQuestionsAction <- match.arg(multipleQuestionsAction,
                                       c("exclude", "keep"))
  if (!is.null(numberOfItems)) {
    stopifnot(is.data.frame(numberOfItems),
              all(screenIdColumns %in% names(numberOfItems)),
              "nItems" %in% names(numberOfItems))
    stopifnot(all(!duplicated(select(numberOfItems, all_of(screenIdColumns)))))
    numberOfItems <- actions %>%
      select(all_of(screenIdColumns)) %>%
      distinct() %>%
      left_join(numberOfItems, by = screenIdColumns)
    stopifnot(all(!is.na(numberOfItems$nItems)))
  } else {
    stopifnot(all(c("questionCode", "subquestionCode") %in% names(actions)))
  }

  message("Preprocessing log-data streams.")
  if (!("broken" %in% names(actions))) {
    actions$broken <- 0
  }
  actions <- actions %>%
    select({{respId}}, {{screenId}},
           all_of(c("type", "timeStampRel", "broken")),
           any_of(c("questionCode", "subquestionCode"))) %>%
    group_by(across(c({{respId}}, {{screenId}}))) %>%
    mutate(broken = any(.data$broken != 0),
           brokenTimeStamps = any(.data$timeStampRel < 0))
  actions <- bind_rows(
    actions %>%
      filter(.data$type %in% "change"),
    actions %>%
      mutate(timeStampRel = last(.data$timeStampRel)) %>%
      filter(.data$type %in% "change") %>%
      slice_tail(n = 1) %>%
      mutate(type = "end",
             across(any_of(c("questionCode", "subquestionCode")),
                           ~"")))

  if (any(actions$broken | actions$brokenTimeStamps)) {
    nRemoved <- nrow(actions)
    mRemoved <- actions %>%
      select(c({{respId}}, {{screenId}})) %>%
      distinct() %>%
      nrow()
    actions <- actions %>%
      filter(!.data$broken, !.data$brokenTimeStamps)
    nRemoved <- nRemoved - nrow(actions)
    mRemoved <- mRemoved - (actions %>%
                              select(c({{respId}}, {{screenId}})) %>%
                              distinct() %>%
                              nrow())
    message(format(mRemoved, big.mark = "'"),
            " respondent-screens containing broken records (",
            format(nRemoved, big.mark = "'"),
            " records) were removed before computing average response times.\n")
  }

  message("Computing number of items on respondent-screens.")
  numberOfItems <-
    count_number_of_items_internal(actions %>%
                                     filter(.data$type == "change"),
                                   {{respId}}, {{screenId}}, numberOfItems)

  message("Computing answering time indicators.")
  actions <- actions %>%
    group_by(across(c({{respId}}, {{screenId}}))) %>%
    arrange(across(c({{respId}}, {{screenId}}, all_of("timeStampRel")))) %>%
    summarise(timeEnd = .data$timeStampRel[.data$type == "end"],
              timeFirstChange = first(.data$timeStampRel),
              timeLastChange = last(.data$timeStampRel[.data$type == "change"]),
              .groups = "keep") %>%
    left_join(numberOfItems,
              by = c(respIdColumns, screenIdColumns))
  if ("nItems" %in% names(actions)) {
    resultsLocal <- actions %>%
      summarise(averageAnsweringTime =
                  (.data$timeLastChange - .data$timeFirstChange) /
                  (.data$nItems - 1) / 1000,
                pageTimeIndex = .data$timeEnd / .data$nItems / 1000,
                .groups = "drop")
  } else {
    resultsLocal <- actions %>%
      ungroup() %>%
      select({{respId}}, {{screenId}}) %>%
      slice(0)
  }
  actions <- actions %>%
    group_by(across(c({{respId}}, {{screenId}}, any_of("nQuestions")))) %>%
    summarise(totalTime = .data$timeEnd / 1000,
              timeToFirstAnswer = .data$timeFirstChange / 1000,
              averageAnsweringTimeAll =
                (.data$timeLastChange - .data$timeFirstChange) /
                (.data$nItemsGlobal - 1) / 1000,
              pageTimeIndexAll = .data$timeEnd / .data$nItemsGlobal / 1000,
              .groups = "drop") %>%
    left_join(resultsLocal,
              by = c(respIdColumns, screenIdColumns))
  if (multipleQuestionsAction == "exclude" &&
      !("nQuestions" %in% names(actions))) {
    warning("If column 'questionCode' is not present in the input data, screens including multiple questions can not be identified and excluded from the analysis.\n(You may set argument `multipleQuestionsAction` to 'keep' not to see this warning).")
  } else if (multipleQuestionsAction == "exclude") {
    actions <- actions %>%
      mutate(across(c(starts_with("averageAnsweringTime"),
                      starts_with("pageTimeIndex")),
                    ~ifelse(!(nQuestions %in% 1L), NA_real_, .)))
  }
  actions <- actions %>%
    select({{respId}}, {{screenId}},
           any_of(c("timeToFirstAnswer",
                    "averageAnsweringTime", "pageTimeIndex",
                    "averageAnsweringTimeAll", "pageTimeIndexAll")))

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
#' @describeIn compute_aat Computing answering time indicators
#' @param hoverTimes A data frame returned by \code{\link{compute_hovering}}.
#' @seealso \code{\link{compute_hovering}}
#' @export
compute_aht <- function(hoverTimes,
                        respId = any_of(c("id", "token", "respid")),
                        screenId = all_of("screen"),
                        returnFormat = c("long", "wide"),
                        numberOfItems = NULL,
                        multipleQuestionsAction = c("exclude", "keep")) {
  stopifnot(is.data.frame(hoverTimes),
            all(c("elementType", "hoverTime") %in% names(hoverTimes)))
  hoverTimes <- ungroup(hoverTimes)
  respIdColumns <- names(select(hoverTimes, {{respId}}))
  screenIdColumns <- names(select(hoverTimes, {{screenId}}))
  returnFormat <- match.arg(returnFormat,
                            c("long", "wide"))
  multipleQuestionsAction <- match.arg(multipleQuestionsAction,
                                       c("exclude", "keep"))
  if (!is.null(numberOfItems)) {
    stopifnot(is.data.frame(numberOfItems),
              all(screenIdColumns %in% names(numberOfItems)),
              "nItems" %in% names(numberOfItems))
    stopifnot(all(!duplicated(select(numberOfItems, all_of(screenIdColumns)))))
    numberOfItems <- hoverTimes %>%
      select(all_of(screenIdColumns)) %>%
      distinct() %>%
      left_join(numberOfItems, by = screenIdColumns)
    stopifnot(all(!is.na(numberOfItems$nItems)))
  } else {
    stopifnot(all(c("questionCode", "subquestionCode") %in% names(hoverTimes)))
  }

  message("Computing number of items on respondent-screens.")
  numberOfItems <-
    count_number_of_items_internal(hoverTimes %>%
                                     filter(!is.na(.data$questionCode)),
                                   {{respId}}, {{screenId}}, numberOfItems)

  message("Computing answering time indicators.")
  hoverTimes <- hoverTimes %>%
    left_join(numberOfItems,
              by = c(respIdColumns, screenIdColumns)) %>%
    group_by(across(c({{respId}}, {{screenId}},
                      any_of("nQuestions")))) %>%
    summarise(totalTime = sum(.data$hoverTime),
              averageHoverTimeAll =
                sum(.data$hoverTime[
                  .data$elementType %in% c("subquestion content",
                                           "answer cell",
                                           "answer control",
                                           "answer text",
                                           "slider bar",
                                           "slider handle")]) / .data$nItemsGlobal[1],
              pageTimeIndexAll = sum(.data$hoverTime) / .data$nItemsGlobal[1],
              .groups = "drop")
  if (multipleQuestionsAction == "exclude" &&
      !("nQuestions" %in% names(hoverTimes))) {
    warning("If column 'questionCode' is not present in the input data, screens including multiple questions can not be identified and excluded from the analysis.\n(You may set argument `multipleQuestionsAction` to 'keep' not to see this warning).")
  } else if (multipleQuestionsAction == "exclude") {
    hoverTimes <- hoverTimes %>%
      mutate(across(c(starts_with("averageHoverTime"),
                      starts_with("pageTimeIndex")),
                    ~ifelse(!(nQuestions %in% 1L), NA_real_, .)))
  }
  hoverTimes <- hoverTimes %>%
    select(-any_of("nQuestions"))

  if (returnFormat == "wide") {
    message("Pivoting results to wide format.")
    hoverTimes <- hoverTimes %>%
      pivot_wider(id_cols = respIdColumns, names_from = screenIdColumns,
                  names_sep = "_",
                  values_from = setdiff(names(hoverTimes), c(respIdColumns,
                                                             screenIdColumns)))
  }
  return(hoverTimes)
}
#' @title Determining numer of items (subquestions) within questions
#' @description Internal function used to determine number of items
#' (subquestions) within questions to be used as denominator while computing
#' average answering time indicators by \code{\link{compute_aat}} and
#' \code{\link{compute_aht}}.
#' @param x A data frame with adequately filtered \emph{actions} (or similar
#' records).
#' @return A data frame with columns:
#' \describe{
#'   \item{respId}{Column(s) defined by \code{respId}.}
#'   \item{screenId}{Column(s) defined by \code{screenId}.}
#'   \item{nItems}{Number of items on the survey screen for a given respondent.
#'                 Included only if the input data included columns
#'                 \emph{questionCode} and \emph{subquestionCode}.}
#'   \item{nQuestions}{Number of questions on the survey screen for a given
#'                     respondent. Included only if the input data included
#'                     columns \emph{questionCode} and \emph{subquestionCode}.}
#'   \item{nItemsGlobal}{Either maximum of \emph{nQuestions} among all the
#'                       respondents (if argument \code{numberOfItem} is
#'                       \code{NULL}) or value of the \emph{nItems} column in
#'                       a data frame provided by the \code{numberOfItem}
#'                       argument.}
#' }
#' @inheritParams compute_aat
#' @noRd
count_number_of_items_internal <- function(x, respId, screenId, numberOfItems) {
  screenIdColumns <- names(select(ungroup(x), {{screenId}}))
  if (all(c("questionCode", "subquestionCode") %in% names(x))) {
    numberOfItemsLocal <- x %>%
      group_by(across(c({{respId}}, {{screenId}}, all_of("questionCode")))) %>%
      mutate(anySubquestionsCodeNotNA = any(!is.na(.data$subquestionCode))) %>%
      filter(!(is.na(.data$subquestionCode) & .data$anySubquestionsCodeNotNA)) %>%
      summarise(nSubquestions = n_distinct(.data$subquestionCode),
                .groups = "drop_last") %>%
      summarise(nItems = sum(.data$nSubquestions),
                nQuestions = n_distinct(.data$questionCode),
                .groups = "drop")
  } else {
    numberOfItemsLocal <- x %>%
      select({{respId}}, {{screenId}}) %>%
      slice(0)
  }
  if (is.null(numberOfItems)) {
    numberOfItems <- numberOfItemsLocal %>%
      group_by(across({{screenId}})) %>%
      summarise(nItemsGlobal = max(.data$nItems),
                .groups = "drop")
  } else {
    numberOfItems <- numberOfItems %>%
      rename(nItemsGlobal = .data$nItems)
  }
  numberOfItemsLocal %>%
    left_join(numberOfItems, by = screenIdColumns) %>%
    return()
}
#' @title Counting number of items on survey screens
#' @description This function may be used to prepare the \code{nItems} argument
#' to the \code{\link{compute_aat}} or \code{\link{compute_aht}} function call.
#'
#' While counting number of \emph{items}:
#' \itemize{
#'   \item{In questions containing subquestions each subquestion counts as
#'         a single \emph{item}.}
#'   \item{Each question that does not contain subquestions counts as a single
#'         \emph{item}.}
#' }
#' @param x A data frame containing survey form INPUT elements' positions
#' (particularly the \emph{inputPositions} element of a list returned by
#' \code{\link{separate_logdata_types}}) or some other data frame
#' containing survey screen id, \emph{questionCode} and \emph{subquestionCode}
#' variables.
#' @inheritParams compute_aat
#' @return A data frame with columns:
#' \describe{
#'   \item{screenId}{Column(s) defined by \code{screenId}.}
#'   \item{nQuestions}{Number of questions on a given survey screen.}
#'   \item{nItes}{Number of \emph{items} on a given survey screen.}
#' }
#' @seealso \code{\link{separate_logdata_types}}
#' @export
count_number_of_items <- function(x, screenId = all_of("screen")) {
  stopifnot(is.data.frame(x),
            all(c("questionCode", "subquestionCode") %in% names(x)))
  screenIdColumns <- names(select(x, {{screenId}}))
  x %>%
    select({{screenId}}, all_of(c("questionCode", "subquestionCode")))
  stopifnot(all(!duplicated(x)))
  x %>%
    group_by(across(c({{screenId}}, all_of("questionCode")))) %>%
    mutate(anySubquestionsCodeNotNA = any(!is.na(.data$subquestionCode))) %>%
    filter(!(is.na(.data$subquestionCode) & .data$anySubquestionsCodeNotNA)) %>%
    summarise(nSubquestions = n_distinct(.data$subquestionCode),
              .groups = "drop_last") %>%
    summarise(nItems = sum(.data$nSubquestions),
              nQuestions = n_distinct(.data$questionCode),
              .groups = "drop") %>%
    return()
}
