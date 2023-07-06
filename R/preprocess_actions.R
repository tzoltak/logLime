#' @title Performing preprocessing of events
#' @description Function is called internally by [separate_logdata_types] and
#' is not exported (i.e. it is not intended to be called by package users
#' themselves). It selects records describing respondent *actions* (i.e. actual
#' events) and performs a preprocessing that includes:
#' \itemize{
#'   \item{Converting columns that are supposed to store numeric values to
#'         a numeric type and substituting "undefined" values with `NAs`.}
#'   \item{Computing length of scrolling events (given information about
#'         *page offsets* that is written in log-data streams).}
#'   \item{Converting *mousemove* events - that actually report a snapshot
#'         of a cursor position (that's how this works in JavaScript) - into
#'         events that actually describe moves.}
#'   \item{Only if `surveyStructure` is provided: assigning question,
#'         subquestion (item) and response category codes to events connected
#'         with *hovering*.}
#'   \item{Assigning labels broadly describing type of an element on which
#'         a given event was triggered.}
#' }.
#' @param logData A data frame with log-data as returned by
#' \code{\link{logstreams_to_data_frame}}.
#' @param respId <[tidy-select][dplyr::dplyr_tidy_select]> Variable(s)
#' identifying respondent.
#' @param screenId <[tidy-select][dplyr::dplyr_tidy_select]> Variable(s)
#' identifying survey screen.
#' @param surveyStructure A data frame storing mapping of *SGQA identifiers*
#' onto question, subquestion and answer codes.
#' @return A data frame with columns:
#' \describe{
#'   \item{respId}{Column(s) defined by `respId`.}
#'   \item{screenId}{Column(s) defined by `screenId`.}
#'   \item{timeStamp}{Time stamp of an event.}
#'   \item{timeStampRel}{Time stamp of an event - relative to the time of
#'                       loading a given survey screen. If there are multiple
#'                       *entries* (returns to a given survey screen), this
#'                       time will be counted relatively to the moment of
#'                       entering the screen for the first time, **subtracting
#'                       the time spent on other survey screens**.}
#'   \item{type}{Type of event.}
#'   \item{target.tagName}{Type of a HTML element that triggered an event (may
#'                         be empty for some types of events).}
#'   \item{target.id}{Id of a HTML element that triggered an event (may be
#'                    empty for some types of events).}
#'   \item{target.class}{CSS classes assigned to a HTML element that triggered
#'                       an event (may be empty for some types of events).}
#'   \item{which}{Information about which mouse button was pressed.}
#'   \item{metaKey}{Information regrading a meta key (ALT, CTRL) was pressed.}
#'   \item{pageX}{Location of an event on the webpage - horizontal axis.}
#'   \item{pageY}{Location of an event on the webpage - vertical axis.}
#'   \item{broken}{Flag indicating whether a record is broken.}
#'   \item{moveX}{Distance of a *mousemove* or *scroll* event on horizontal
#'                axis.}
#'   \item{moveY}{Distance of a *mousemove* or *scroll* event on vertical axis.}
#'   \item{duration}{Duration of a *mousemove* event.}
#'   \item{moveXScrollCorrected}{Distance of a *mousemove* event on
#'                               horizontal axis, corrected for scrolling.}
#'   \item{moveYScrollCorrected}{Distance of a *mousemove* event on
#'                               vertical axis, corrected for scrolling.}
#'   \item{elementType}{Broad type of a survey interface element that triggered
#'                      an event.}
#' }
#'
#' Additionally, if `surveyStructure` argument was provided also with columns:
#' \describe{
#'   \item{surveyId}{Survey id (in LimeSurvey database).}
#'   \item{questionId}{Question id (in LimeSurvey database).}
#'   \item{questionFormat}{A letter code of a question format - see
#'                         \href{https://manual.limesurvey.org/Question_object_types#Current_question_types}{LimeSurvey documentation page}.}
#'   \item{SGQA}{Question/subquestion/answer *SGQA identifier* - see
#'               \href{https://manual.limesurvey.org/SGQA_identifier}{LimeSurvey documentation page}.}
#'   \item{questionCode}{Question code.}
#'   \item{subquestionCode}{Subquestion code.}
#'   \item{answerCode}{Answer code.}
#' }
#' @seealso [separate_logdata_types] [separate_returns] [read_survey_structure]
#' @importFrom dplyr %>% .data arrange bind_rows case_when everything
#' filter group_by if_else lag lead mutate n pick reframe ungroup
preprocess_actions <- function(logData, respId, screenId,
                               surveyStructure = NULL) {
  # there will be probably some warnings in conversion to numerics because of
  # a cut "undefined" in some broken records - that's why suppressWarnings()
  logData <- suppressWarnings(
    logData %>%
      filter(!(.data$type %in% c("browser", "screen", "input_position"))) %>%
      mutate(across(c("target.tagName", "target.id", "target.class", "which",
                      "metaKey", "pageX", "pageY"),
                    ~if_else(. %in% c("", "undefined"), NA_character_, .)),
             across(c("timeStamp", "which", "pageX", "pageY"), as.numeric),
             metaKey = as.numeric(as.logical(.data$metaKey))) %>%
      mutate(rowPosition = seq_len(n()))
  )
  if (any(logData$entry > 1L)) {
    logData <- logData %>%
      group_by(across(c({{respId}}, {{screenId}}, "entry"))) %>%
      mutate(timeStampMin = ifelse(first(.data$type %in% "pageLoaded"),
                                   first(.data$timeStamp), NA_real_),
             timeStampMax = ifelse(last(.data$type %in% "submit"),
                                   last(.data$timeStamp), NA_real_)) %>%
      group_by(across(c({{respId}}, {{screenId}}))) %>%
      mutate(timeStampOffset =
               cumsum(ifelse(.data$entry > lag(.data$entry, default = 0),
                             .data$timeStampMin - lag(.data$timeStampMax,
                                                      default = 0),
                             0)),
             timeStampRel = .data$timeStamp - .data$timeStampOffset) %>%
      ungroup() %>%
      select({{respId}}, {{screenId}}, "timeStamp", "timeStampRel",
             everything(), -c("timeStampMin", "timeStampMax", "timeStampOffset"))
  } else {
    logData <- logData %>%
      group_by(across(c({{respId}}, {{screenId}}, "entry"))) %>%
      mutate(timeStampRel = .data$timeStamp -
               c(.data$timeStamp[.data$type %in% "pageLoaded"],
                 .data$timeStamp[1])[1]) %>%
      ungroup() %>%
      select({{respId}}, {{screenId}}, "timeStamp", "timeStampRel",
             everything())
  }
  message("  Computing scroll lengths...")
  logData <- compute_scrolls(logData, {{respId}}, {{screenId}}, "entry")
  message("  Transforming mousemove events into actual moves...")
  logData <- compute_mouse_moves(logData, {{respId}}, {{screenId}}, "entry")
  if (!is.null(surveyStructure)) {
    message("  Labeling clicks and hovers...")
    logData <- logData %>%
      left_join(surveyStructure, by = "target.id")
  } else {
    message("  With no `surevyStructure` argument provided 'actions' can not be linked to the specific questions, subquestions and answers.")
  }
  message("  Labelling types of elements...")
  logData <- label_actions(logData)
  message("Done.")
  logData %>%
    select(-"rowPosition") %>%
    return()
}
#' @title Computing scroll lengths
#' @description Function computes length of scrolls given a sequence of
#' *scroll* events
#' @inheritParams preprocess_actions
#' @return A data frame.
#' @noRd
compute_scrolls <- function(actions, respId, screenId, entryId) {
  bind_rows(actions %>%
              filter(!(.data$type %in% "scroll")),
            actions %>%
              filter(.data$type %in% "scroll") %>%
              group_by(across(c({{respId}}, {{screenId}}, {{entryId}}))) %>%
              mutate(moveX =
                       .data$pageX - lag(.data$pageX, default = 0),
                     moveY =
                       .data$pageY - lag(.data$pageY, default = 0)) %>%
              ungroup()) %>%
    arrange(.data$rowPosition) %>%
    return()
}
#' @title Computing cursor move lengths
#' @description Function computes length of cursor moves given a sequence of
#' *mousemove* and *scroll* events
#' @inheritParams preprocess_actions
#' @return A data frame.
#' @noRd
compute_mouse_moves <- function(actions, respId, screenId, entryId) {
  bind_rows(actions %>%
              filter(!(.data$type %in% "mousemove")),
            actions %>%
              filter(.data$type %in% c("mousemove", "pageLoaded")) %>%
              group_by(across(c({{respId}}, {{screenId}}, {{entryId}}))) %>%
              mutate(pageX = ifelse(.data$type %in% "pageLoaded",
                                    lead(.data$pageX), .data$pageX),
                     pageY = ifelse(.data$type %in% "pageLoaded",
                                    lead(.data$pageY), .data$pageY),
                     duration = .data$timeStamp - lag(.data$timeStamp),
                     moveX = .data$pageX - lag(.data$pageX, default = 0),
                     moveY = .data$pageY - lag(.data$pageY, default = 0),
                     moveXScrollCorrected = .data$moveX,
                     moveYScrollCorrected = .data$moveY) %>%
              bind_rows(actions %>%
                          filter(.data$type %in% "scroll")) %>%
              arrange(.data$rowPosition) %>%
              reframe(pick(everything()) %>%
                        (function(x) {
                          if (nrow(x) < 2) return(x)
                          for (r in 2:nrow(x)) {
                            if (all(x$type[c(r - 1, r)] %in% "scroll")) {
                              x$moveX[r] <- sum(x$moveX[c(r - 1, r)])
                              x$moveY[r] <- sum(x$moveY[c(r - 1, r)])
                            } else if (x$type[r] %in% "mousemove" && x$type[r - 1] %in% "scroll") {
                              x$moveXScrollCorrected[r] <-
                                x$moveXScrollCorrected[r] - x$moveX[r - 1]
                              x$moveYScrollCorrected[r] <-
                                x$moveYScrollCorrected[r] - x$moveY[r - 1]
                            }
                          }
                          return(x)
                        })) %>%
              filter(.data$type %in% "mousemove")) %>%
    arrange(.data$rowPosition) %>%
    return()
}
#' @title Labelling events with a broad type of survey interface element types
#' @description Function assigns to events a label describing a broad type of
#' survey interface element type.
#' @inheritParams preprocess_actions
#' @return The input data frame with a column *elementType* added.
#' @noRd
label_actions <- function(logData) {
  logData <- logData %>%
    mutate(elementType = case_when(
      target.id %in% "ls-button-submit" ~ "submit button",
      target.id %in% "navbar" ~ "top bar",
      grepl("^question[[:digit:]]+$|^ls-question-text-", target.id) ~ "question content",
      grepl("^javatbd", target.id) & target.tagName %in% "LI" & grepl("button", target.class) ~ "answer button",
      grepl("^javatbd", target.id) & target.tagName %in% "LI" & grepl("question", target.class) ~ "subquestion content",
      grepl("^javatbd", target.id) & target.tagName %in% "LI" ~ "answer line",
      grepl("^javatbd", target.id) & target.tagName %in% "DIV" ~ "answer button",
      grepl("^(javatbd|answertext)", target.id) & target.tagName %in% c("TH", "TD") ~ "subquestion content",
      grepl("^label", target.id) & target.tagName %in% "LABEL" ~ "subquestion content",
      grepl("^answer", target.id) & target.tagName %in% "LI" ~ "subquestion content",
      grepl("^answer", target.id) & target.tagName %in% "TD" ~ "answer cell",
      grepl("^answer", target.id) & target.tagName %in% "INPUT" & type %in% "change" ~ "answer control",
      target.class %in% "answer-text" ~ "response scale (arrays)",
      grepl("slid-container$", target.id) ~ "slider bar",
      grepl("slider-(handle)", target.class) ~ "slider handle",
      grepl("slider", target.class) ~ "slider bar",
      grepl("ls-questionhelp", target.class) ~ "question help"
    ))
  if ("questionFormat" %in% names(logData)) {
    logData %>%
      mutate(elementType = case_when(
        grepl("^answer", target.id) & target.tagName %in% "LABEL" &
          questionFormat %in% c("1", "A", "B", "C", "E", "F", "H", ";", ";") ~ "answer control",
        grepl("^answer", target.id) & target.tagName %in% "LABEL" ~ "answer text",
        grepl("^answer", target.id) & target.tagName %in% "INPUT" &
          questionFormat %in% c("1", "A", "B", "C", "E", "F", "H", ";", ";") ~ "answer cell",
        grepl("^answer", target.id) & target.tagName %in% "INPUT" ~ "answer control",
        TRUE ~ elementType
      )) %>%
      return()
  } else {
    logData %>%
      mutate(elementType = case_when(
        grepl("^answer", target.id) & target.tagName %in% "LABEL" ~ "answer text or control",
        grepl("^answer", target.id) & target.tagName %in% "INPUT" ~ "answer control or cell",
        TRUE ~ elementType
      )) %>%
      return()
  }
}
