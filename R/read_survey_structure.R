#' @title Reading in text files storing survey structure
#' @description Function is called internally by
#' \code{\link{separate_logdata_types}} and is not exported (i.e. it is not
#' intended to be called by package users themselves). It reads in text
#' (tab-delimited) files storing survey structure previously exported from the
#' \emph{LimeSurvey} web-survey platform (if provided with file names) or passes
#' down a provided data frame.
#' @param x character vector with file names \strong{or} a data frame with
#' already read survey structure file
#' @return data frame with columns:
#' \itemize{
#'   \item{target.id,}
#'   \item{surveyId,}
#'   \item{questionId,}
#'   \item{questionFormat,}
#'   \item{SGQA,}
#'   \item{questionCode,}
#'   \item{subquestionCode,}
#'   \item{answerCode.}
#' }
#' @importFrom utils read.delim
#' @importFrom dplyr %>% .data all_of bind_rows filter left_join mutate
#' select summarise
#' @importFrom tidyr expand_grid
read_survey_structure <- function(x) {
  if (is.character(x)) {
    fExists <- file.exists(x)
    if (!all(fExists)) {
      stop("File(s) with survey structure that do not exist: '",
           paste(x[!fExists], collapse = "', '"), "'.")
    }
    names(x) <- x
    x <- suppressWarnings(lapply(x, function(x) {
      return(try(read.delim(x, header = TRUE, sep = "\t", encoding = "UTF-8",
                            stringsAsFactors = FALSE),
                 silent = TRUE))
    }))
    fProblems <- sapply(x, inherits, what = "try-error")
    if (any(fProblems)) {
      stop("File(s) with survey structure that can not be readed: '",
           paste(names(x)[fProblems], collapse = "', '"), "'.")
    }
  } else if (is.data.frame(x)) {
    x <- list(x)
  }

  x <- lapply(x,
              function(x) {
                try(discard_nonimportant_survey_information(x) %>%
                      return())
              })
  fProblems <- sapply(x, inherits, what = "try-error")
  if (any(fProblems)) {
    stop("Aforementioned error(s) occured while processing file(s): '",
         paste(names(x)[fProblems], collapse = "', '"), "'.")
  }

  lapply(x, construct_ids) %>%
    bind_rows() %>%
    return()
}
discard_nonimportant_survey_information <- function(x) {
  names(x) <- sub("^type.?[sS]cale$", "questionFormat", names(x))
  x %>%
    select(all_of(c("id", "class", "name", "text", "questionFormat"))) %>%
    mutate(surveyId = .data$text[.data$name == "sid"]) %>%
    filter(class %in% c("G", "Q", "SQ", "A")) %>%
    return()
}
construct_ids <- function(x) {
  x$groupId <-  x$questionId <- groupId <- questionId <- NA
  for (r in 1:nrow(x)) {
    if (x$class[r] %in% "G") {
      groupId <- x$id[r]
    }
    if (x$class[r] %in% "Q") {
      questionId <- x$id[r]
    }
    x$groupId[r] <- groupId
    x$questionId[r] <- questionId
  }
  questions <- x %>%
    filter(class %in% "Q") %>%
    mutate(SGQA = paste0(.data$surveyId, "X", .data$groupId, "X",
                         .data$questionId)) %>%
    select(questionCode = .data$name, .data$questionFormat, .data$SGQA,
           .data$surveyId, .data$questionId)
  subquestions <- x %>%
    filter(class %in% "SQ") %>%
    mutate(SGQA = paste0(.data$surveyId, "X", .data$groupId, "X",
                         .data$questionId, .data$name)) %>%
    select(subquestionCode = .data$name, .data$questionFormat, .data$SGQA,
           .data$surveyId, .data$questionId) %>%
    left_join(questions %>%
                select(.data$questionId, .data$questionCode),
              by = "questionId")
  answers <- x %>%
    filter(class %in% "A") %>%
    mutate(SGQA = paste0(.data$surveyId, "X", .data$groupId, "X",
                         .data$questionId, .data$name)) %>%
    select(answerCode = .data$name, .data$questionFormat, .data$SGQA,
           .data$surveyId, .data$questionId) %>%
    left_join(questions %>%
                select(.data$questionId, .data$questionCode),
              by = "questionId")
  subquestionsAnswers <- x %>%
    filter(class %in% c("SQ", "A"))
  subquestionsAnswers <- split(subquestionsAnswers,
                               subquestionsAnswers$questionId) %>%
    lapply(function(x) {
      tidyr::expand_grid(subquestionCode = x$name[x$class %in% "SQ"],
                  answerCode = x$name[x$class %in% "A"]) %>%
        mutate(surveyId = x$surveyId[1],
               groupId = x$groupId[1],
               questionId = x$questionId[1]) %>%
        return()
    }) %>%
    bind_rows() %>%
    mutate(SGQA = paste0(.data$surveyId, "X", .data$groupId, "X",
                         .data$questionId, .data$subquestionCode, "-",
                         .data$answerCode)) %>%
    select(.data$subquestionCode, .data$answerCode, .data$SGQA, .data$surveyId,
           .data$questionId) %>%
    left_join(questions %>%
                select(.data$questionId, .data$questionCode,
                       .data$questionFormat),
              by = "questionId")

  bind_rows(
    questions %>%
      mutate(target.id = paste0("ls-question-text-", .data$SGQA)),
    questions %>%
      mutate(target.id = paste0("answer", .data$SGQA)),
    questions %>%
      mutate(target.id = paste0("java", .data$SGQA)),
    questions %>%
      mutate(target.id = paste0("answer", .data$SGQA, "comment")),
    questions %>%
      mutate(target.id = paste0("question",
                                sub("^[[:digit:]]+X[[:digit:]]+X",
                                    "", .data$SGQA))),
    subquestions %>%
      mutate(target.id = paste0("answer", .data$SGQA)),
    subquestions %>%
      mutate(target.id = paste0("answertext", .data$SGQA)),
    subquestions %>%
      mutate(target.id = paste0("javatbd", .data$SGQA)),
    subquestions %>%
      mutate(target.id = paste0("javatbd", .data$SGQA, "comment")),
    subquestions %>%
      mutate(target.id = paste0("label-", .data$SGQA)),
    subquestions %>%
      mutate(target.id = paste0("answer", .data$SGQA, "slid-container")),
    subquestionsAnswers %>%
      mutate(target.id = paste0("answer", .data$SGQA)),
    subquestionsAnswers %>%
      mutate(target.id = paste0("answer", sub("-([^-]+)$", "_0-\\1", .data$SGQA))),
    subquestionsAnswers %>%
      mutate(target.id = paste0("answer", sub("-([^-]+)$", "_0-\\1", .data$SGQA))),
    answers %>%
      mutate(target.id = paste0("answer", .data$SGQA)),
    answers %>%
      mutate(target.id = paste0("javatbd", .data$SGQA))
  ) %>%
    select(.data$target.id, .data$surveyId, .data$questionId,
           .data$questionFormat, .data$SGQA, .data$questionCode,
           .data$subquestionCode, .data$answerCode) %>%
    return()
}
