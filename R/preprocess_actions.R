#' @title Performing preprocessing of events
#' @description Function is called internally by
#' \code{\link{separate_logdata_types}} and is not exported (i.e. it is not
#' intended to be called by package users themselves). It selects records
#' describing respondent \emph{actions} (i.e. actual events) and performs
#' a preprocessing that includes:
#' \itemize{
#'   \item{Converting column types storing numeric values to numeric type and
#'         substituting "undefined" values with \code{NAs}.}
#'   \item{Computing length of scrolling events (given information about
#'         \emph{page offsets} that is written in log-data streams;)}
#'   \item{Converting \emph{mousemove} events - that actually report a snapshot
#'         of a cursor position (that's how this works in JavaScript) - into
#'         events that actually describe moves}
#'   \item{Only if \code{surveyStructure} is provided: assigning question,
#'         subquestion (item) and response category codes to events connected
#'         with \emph{hovering} and provides a simple classification of what was
#'         \emph{hovered}.}
#'   \item{Assigning labels broadly describing type of an element on which
#'         a given event was triggered.}
#' }.
#' @param logData data frame with log-data as returned by
#' \code{\link{logstreams_to_data_frame}}
#' @param respScreenIds <tidy-select> names of columns identifying
#' respondent-screens
#' @param surveyStructure data frame with mapping of \emph{SGQA identifiers}
#' onto question, subquestion and answer codes
#' @return data frame with columns:
#' \itemize{
#'   \item{Columns defined by \code{respScreenIds}}
#'   \item{timeStamp,}
#'   \item{timeStampRel,}
#'   \item{type,}
#'   \item{target.tagName,}
#'   \item{target.id,}
#'   \item{target.class,}
#'   \item{which,}
#'   \item{metaKey,}
#'   \item{pageX,}
#'   \item{pageY,}
#'   \item{broken,}
#'   \item{moveX,}
#'   \item{moveY,}
#'   \item{duration,}
#'   \item{moveXScrollCorrected,}
#'   \item{moveYScrollCorrected,}
#'   \item{elementType.}
#' }
#' and if \code{surveyStructure} argument was provided with additional columns:
#' \itemize{
#'   \item{surveyId,}
#'   \item{questionId,}
#'   \item{questionFormat,}
#'   \item{SGQA,}
#'   \item{questionCode,}
#'   \item{subquestionCode,}
#'   \item{answerCode.}
#' }
#' @importFrom dplyr %>% .data arrange bind_rows case_when cur_data everything
#' filter group_by if_else lag mutate n ungroup
preprocess_actions <- function(logData, respScreenIds, surveyStructure = NULL) {
  # there will be probably some warnings in conversion to numerics because of
  # a cut "undefined" in some broken records - that's why suppressWarnings()
  logData <- suppressWarnings(
    logData %>%
      filter(!(.data$type %in% c("browser", "screen", "input_position"))) %>%
      mutate(across(c(.data$target.tagName, .data$target.id, .data$target.class,
                      .data$which, .data$metaKey, .data$pageX, .data$pageY),
                    ~if_else(. == "undefined", NA_character_, .)),
             across(c(.data$timeStamp, .data$which, .data$pageX, .data$pageY),
                    as.numeric),
             metaKey = as.numeric(as.logical(.data$metaKey))) %>%
      mutate(rowPosition = 1:n()) %>%
      group_by(across({{respScreenIds}})) %>%
      mutate(timeStampRel = .data$timeStamp -
               c(.data$timeStamp[.data$type %in% "pageLoaded"],
                 .data$timeStamp[1])[1]) %>%
      select({{respScreenIds}}, .data$timeStamp, .data$timeStampRel, everything()) %>%
      ungroup()
  )
  message("\nComputing scroll lengths...")
  logData <- compute_scrolls(logData, respScreenIds)
  message("Transforming mousemove events into actual moves...")
  logData <- compute_mouse_moves(logData, respScreenIds)
  if (!is.null(surveyStructure)) {
    message("Labeling clicks and hovers...")
    logData <- logData %>%
      left_join(surveyStructure, by = "target.id")
  } else {
    warning("With no `surevyStructure` argument provided 'actions' can not be linked to the specific questions, subquestions and answers.")
  }
  message("Labelling types of elements...")
  logData <- logData %>%
    mutate(elementType = case_when(
      target.id %in% "ls-button-submit" ~ "submit button",
      target.id %in% "navbar" ~ "top bar",
      grepl("^question[[:digit:]]+$|^ls-question-text-", target.id) ~ "question content",
      grepl("^(javatbd|answertext)", target.id) & target.tagName %in% c("TH", "TD", "LI") ~ "subquestion content",
      grepl("^label", target.id) & target.tagName %in% "LABEL" ~ "subquestion content",
      grepl("^answer", target.id) & target.tagName %in% "LI" ~ "subquestion content",
      grepl("^answer", target.id) & target.tagName %in% "TD" ~ "answer cell",
      grepl("^answer", target.id) & target.tagName %in% "LABEL" &
        questionFormat %in% c("1", "A", "B", "C", "E", "F", "H", ";", ";") ~ "answer control",
      grepl("^answer", target.id) & target.tagName %in% "LABEL" ~ "answer text",
      grepl("^answer", target.id) & target.tagName %in% "INPUT" & type %in% "change" ~ "answer control",
      grepl("^answer", target.id) & target.tagName %in% "INPUT" &
        questionFormat %in% c("1", "A", "B", "C", "E", "F", "H", ";", ";") ~ "answer cell",
      grepl("^answer", target.id) & target.tagName %in% "INPUT" ~ "answer control",
      target.class %in% "answer-text" ~ "response scale (arrays)",
      grepl("slid-container$", target.id) ~ "slider bar",
      grepl("slider-(handle)", target.class) ~ "slider handle",
      grepl("slider", target.class) ~ "slider bar",
      grepl("ls-questionhelp", target.class) ~ "question help"
    )) %>%
    select(-.data$rowPosition)
  message("Done.")
  return(logData)
}
compute_scrolls <- function(actions, respScreenIds) {
  bind_rows(actions %>%
              filter(!(.data$type %in% "scroll")),
            actions %>%
              filter(.data$type %in% "scroll") %>%
              group_by(across({{respScreenIds}})) %>%
              mutate(moveX =
                       .data$pageX - lag(.data$pageX, default = 0),
                     moveY =
                       .data$pageY - lag(.data$pageY, default = 0)) %>%
              ungroup()) %>%
    arrange(.data$rowPosition) %>%
    return()
}
compute_mouse_moves <- function(actions, respScreenIds) {
  bind_rows(actions %>%
              filter(!(.data$type %in% "mousemove")),
            actions %>%
              filter(.data$type %in% c("mousemove", "pageLoaded")) %>%
              group_by(across({{respScreenIds}})) %>%
              mutate(duration = .data$timeStamp - lag(.data$timeStamp),
                     moveX = .data$pageX - lag(.data$pageX, default = 0),
                     moveY = .data$pageY - lag(.data$pageY, default = 0),
                     moveXScrollCorrected = .data$moveX,
                     moveYScrollCorrected = .data$moveY) %>%
              bind_rows(actions %>%
                          filter(.data$type %in% "scroll")) %>%
              arrange(.data$rowPosition) %>%
              summarise(cur_data() %>%
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
                          }),
                        .groups = "drop") %>%
              filter(.data$type %in% "mousemove")) %>%
    arrange(.data$rowPosition) %>%
    return()
}
