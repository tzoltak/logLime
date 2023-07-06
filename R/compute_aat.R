#' @title Computing answering time indicators
#' @description Functions enables to compute several indicators describing
#' respondent's answering speed using either data on time of changing
#' states of the survey form controls ([compute_aat]) or previously
#' computed (using [compute_hovering]) hovering times.
#' @param actions A data frame containing data regarding *actions* -
#' typically element *actions* of a list returned by
#' \code{\link{separate_logdata_types}}.
#' @param respId <[tidy-select][dplyr::dplyr_tidy_select]>  Variable(s)
#' identifying respondent.
#' @param screenId <[tidy-select][dplyr::dplyr_tidy_select]> Variable(s)
#' identifying survey screen.
#' @param returnFormat String indicating whether results should be returned in
#' the *long* (row is respondent-(sub)question) or in the *wide*
#' (row is a respondent, there is a separate column for each (sub)question)
#' format. Can be abbreviated.
#' @param numberOfItems Optionally a data frame containing: survey screen
#' identifier(s) (same as provided by `screenId`) and a column `nItems`
#' specifying number of items on a given screen. The latter will
#' be used as denominator while computing average answering time indicators in
#' their variants labeled *All* (see section on returned results) instead
#' of determining these numbers given data provided by the `actions` (or
#' `hoverTimes`) argument. One may use function [count_number_of_items]
#' to prepare such a data frame.
#' @param multipleQuestionsAction String indicating what to do with
#' respondent-screens containing more than 1 question - default value
#' (*exclude*) means returning `NAs` in average answering time
#' indicators. That is because these measures were developed with respect to the
#' questions in a tabular form presented in a one-question-per-survey screen
#' manner. Nevertheless one may change this argument to *keep* to get
#' indices computed also for screens containing many questions (in such a case
#' for purpose of counting the number of *items* a question with no
#' subquestions defined is treated as a single *item*).
#' @details Functions discard survey-screens for which there are some broken
#' records.
#'
#' Set of indicators that is returned depends on whether input data
#' contains variables *questionCode* and *subquestionCode*, because
#' without these two it can not be determined to which subquestion a given
#' *action* relates to. See section on returned results below.
#' @return If `returnFormat` is *long* a data frame with columns:
#' \describe{
#'   \item{respId}{Column(s) defined by `respId`.}
#'   \item{screenId}{Column(s) defined by `screenId`.}
#'   \item{timeToFirstAnswer}{Time to first answer (change of survey form
#'                            control for some item).
#'                            **Returned only by `compute_aat`.**}
#'   \item{averageAnsweringTime}{Average time spent on answering a single item
#'                               (estimated excluding first item that was
#'                               answered on the survey screen), **taking
#'                               into account only items that were answered**.
#'                               **Returned only by `compute_aat`.**
#'                               Not returned if the input data do not contain
#'                               columns *questionCode* or *subquestionCode*.}
#'   \item{pageTimeIndex}{Screen time (`totalTime`) divided by the number
#'                        of items **that were answered** on a given survey
#'                        screen. **Returned only by `compute_aat`.**
#'                        Not returned if the input data do not contain columns
#'                        *questionCode* or *subquestionCode*.}
#'   \item{averageAnsweringTimeAll}{Average time spent on answering a single
#'                                  item (estimated excluding first item that
#'                                  was answered on the survey screen), but
#'                                  computed by **dividing by the number
#'                                  of all the items on a given survey screen
#'                                  (irrespective they were answered or
#'                                  omitted)**. **Returned only by
#'                                  `compute_aat`.**}
#'   \item{pageTimeIndexAll}{Screen time (`totalTime`) **divided by the number
#'                           of all the items on a given survey screen
#'                           (irrespective they were answered or omitted)**.}
#'   \item{totalTime}{Screen time, i.e. total time spent on the survey screen.
#'                    **Returned only by `compute_aht`.**}
#'   \item{averageHoverTimeAll}{Average time of mouse cursor hovering over
#'                              a single item (taking into account both content
#'                              and answers), taking into account **all the
#'                              items on a given survey screen (irrespective
#'                              they were answered or omitted)**.
#'                              **Returned only by `compute_aht`.**}
#' }
#' If `returnFormat` is *wide*: a data frame with columns:
#' \describe{
#'   \item{respId}{Column(s) defined by `respId`.}
#'   \item{index_screen}{Columns named in a convention *index_screen* where
#'                       *index* is one of the names listed above with
#'                       respect to the *long* format of returned results
#'                       and *screen* is value of the column(s) defined by
#'                       `screenId`.}
#' }
#' @seealso [separate_logdata_types], [separate_returns]
#' @importFrom dplyr %>% .data across all_of any_of bind_rows distinct filter
#' group_by left_join mutate n_distinct rename select
#' slice slice_tail summarise starts_with ungroup
#' @importFrom tidyr pivot_wider
#' @export
compute_aat <- function(actions,
                        respId = any_of(c("id", "token", "respid")),
                        screenId = "screen",
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
    actions$broken <- 0L
  }
  actions <- actions %>%
    select({{respId}}, {{screenId}}, "type", "timeStampRel", "broken",
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
    arrange(across(c({{respId}}, {{screenId}}, "timeStampRel"))) %>%
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
#' @param hoverTimes A data frame returned by [compute_hovering].
#' @seealso [compute_aat]
#' @export
compute_aht <- function(hoverTimes,
                        respId = any_of(c("id", "token", "respid")),
                        screenId = "screen",
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
#' average answering time indicators by [compute_aat] and [compute_aht].
#' @param x A data frame with adequately filtered *actions* (or similar
#' records).
#' @return A data frame with columns:
#' \describe{
#'   \item{respId}{Column(s) defined by `respId`.}
#'   \item{screenId}{Column(s) defined by `screenId`.}
#'   \item{nItems}{Number of items on the survey screen for a given respondent.
#'                 Included only if the input data included columns
#'                 *questionCode* and *subquestionCode*.}
#'   \item{nQuestions}{Number of questions on the survey screen for a given
#'                     respondent. Included only if the input data included
#'                     columns *questionCode* and *subquestionCode*.}
#'   \item{nItemsGlobal}{Either maximum of *nQuestions* among all the
#'                       respondents (if argument `numberOfItem` is `NULL`)
#'                       or value of the *nItems* column in a data frame
#'                       provided by the `numberOfItem` argument.}
#' }
#' @inheritParams compute_aat
#' @noRd
count_number_of_items_internal <- function(x, respId, screenId, numberOfItems) {
  screenIdColumns <- names(select(ungroup(x), {{screenId}}))
  if (all(c("questionCode", "subquestionCode") %in% names(x))) {
    numberOfItemsLocal <- x %>%
      group_by(across(c({{respId}}, {{screenId}}, "questionCode"))) %>%
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
      slice(0L)
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
#' @description This function may be used to prepare the `nItems` argument
#' to the [compute_aat] or [compute_aht] function call.
#'
#' While counting number of *items*:
#' \itemize{
#'   \item{In questions containing subquestions each subquestion counts as
#'         a single *item*.}
#'   \item{Each question that does not contain subquestions counts as a single
#'         *item*.}
#' }
#' @param x A data frame containing survey form INPUT elements' positions
#' (particularly the *inputPositions* element of a list returned by
#' [separate_logdata_types]) or some other data frame containing survey screen
#' id, *questionCode* and *subquestionCode* variables.
#' @inheritParams compute_aat
#' @return A data frame with columns:
#' \describe{
#'   \item{screenId}{Column(s) defined by `screenId`.}
#'   \item{nQuestions}{Number of questions on a given survey screen.}
#'   \item{nItes}{Number of *items* on a given survey screen.}
#' }
#' @seealso [separate_logdata_types]
#' @export
count_number_of_items <- function(x, screenId = "screen") {
  stopifnot(is.data.frame(x),
            all(c("questionCode", "subquestionCode") %in% names(x)))
  x %>%
    select({{screenId}}, "questionCode", "subquestionCode")
  stopifnot(all(!duplicated(x)))
  x %>%
    group_by(across(c({{screenId}}, "questionCode"))) %>%
    mutate(anySubquestionsCodeNotNA = any(!is.na(.data$subquestionCode))) %>%
    filter(!(is.na(.data$subquestionCode) & .data$anySubquestionsCodeNotNA)) %>%
    summarise(nSubquestions = n_distinct(.data$subquestionCode),
              .groups = "drop_last") %>%
    summarise(nItems = sum(.data$nSubquestions),
              nQuestions = n_distinct(.data$questionCode),
              .groups = "drop") %>%
    return()
}
