#' @title Performing preprocessing of input positions
#' @description Function is called internally by
#' \code{\link{separate_logdata_types}} and is not exported (i.e. it is not
#' intended to be called by package users themselves). It selects records
#' describing input element's positions from a data frame storing different
#' types of log-data (returned by \code{\link{logstreams_to_data_frame}}) and
#' performs a preprocessing that includes:
#' \itemize{
#'   \item{Converting column types storing numeric values to numeric type.}
#'   \item{Transforming \code{pageX} and \code{pageY} columns so they describe
#'         position of the \strong{center} of the element instead of position
#'         of its upper-left corner.}
#'   \item{If  \code{surveyStructure} argument is provided, transforming
#'         \code{pageX} and \code{pageY} columns
#'          \strong{for inputs from array-type questions} so they describe
#'          actual position of an element on the page (there are some
#'          limitations - see \emph{Details} section below).}
#' }
#' Last two transformations make returned data more useful while creating
#' backgrounds on plots showing cursor moves.
#' @param logData data frame with log-data as returned by
#' \code{\link{logstreams_to_data_frame}}
#' @param respScreenIds <tidy-select> names of columns identifying
#' respondent-screens
#' @param surveyStructure optionally a data frame with mapping of values
#' possibly occurring in the \code{target.id} column of a data frame provided by
#' the \code{logData} argument onto question, subquestion and answer codes as
#' returned by \code{\link{read_survey_structure}}
#' @param imputeLastPageXY function that will be used to impute position of
#' inputs in last subquestions/answers in array-format questions - see
#' \emph{Details} section below; it is \code{\link{median}} by default
#' @details Because of some aspects of how \emph{LimeSurvey's} responsive
#' layout works with respect to array type questions in these questions position
#' of INPUT elements do not describe actual position at which INPUT element were
#' visible for the respondent (i.e. center of an array cell; instead, these
#' positions describe the upper-left corner of a table cell in which a given
#' element was placed). \strong{Function can identify array type questions only
#' if \code{respScreenIds} argument is provided} and then it tries to correct
#' reported positions so they describe actual position of INPUT elements on the
#' page. However, there are some limitations:
#' \itemize{
#'   \item{Precise position can be derived for every answer/subquestion except
#'         the last ones by subtracting position reported for inputs associated
#'         with the next answer/subquestion from position of inputs associated
#'         with a given answer/subquestion (and dividing such a difference
#'         by 2).}
#'   \item{Position of the last answer/subquestion can not be derived and must
#'         be imputed. Size of the correction is computed using a function
#'         provided by \code{imputeLastPageXY} argument - \code{\link{median}}
#'         by default. This function is called on a vector of corrections that
#'         were already computed for the other answers/subquestions. With
#'         respect to the horizontal axis such imputation typically should be
#'         precise because \emph{LimeSurvey} rather creates table cells of the
#'         same width in array type questions. \strong{With respect to the
#'         vertical axis, it is nothing more than a lucky guess, however.}}
#'   \item{\strong{If there is only one answer or subquestion, position on
#'         a respective dimension can not be corrected (because of the lack of
#'         data) and it is therefore left unchanged.}}
#'   \item{AT the moment \strong{no corrections are made for an array
#'         dual-scale questions}.}
#' }
#' @return data frame with columns:
#' \itemize{
#'   \item{Columns defined by \code{respScreenIds}}
#'   \item{target.tagName,}
#'   \item{target.id,}
#'   \item{target.class,}
#'   \item{width,}
#'   \item{height,}
#'   \item{pageX,}
#'   \item{pageY.}
#' }
#' and if \code{surveyStructure} argument was provided with additional columns:
#' \itemize{
#'   \item{questionId,}
#'   \item{questionFormat,}
#'   \item{subquestionCode,}
#'   \item{answerCode.}
#' }
#' @importFrom stats median
#' @importFrom dplyr %>% .data all_of anti_join filter inner_join mutate n
#' select
preprocess_input_positions <- function(logData, respScreenIds,
                                       surveyStructure = NULL,
                                       imputeLastPageXY = median) {
  logData <- logData %>%
    filter(.data$type %in% c("input_position")) %>%
    select({{respScreenIds}}, .data$target.tagName, .data$target.id,
           .data$target.class, width = .data$which, height = .data$metaKey,
           .data$pageX, .data$pageY)
  if (all(is.na(logData$pageY))) { # correcting for a bug in early version of the JS applet
    logData <- logData %>%
      mutate(pageY = .data$pageX,
             pageX = sub("^.*,", "", .data$height),
             height = sub(",.*$", "", .data$height))
  }
  logData <- logData %>%
    mutate(across(all_of(c("width", "height", "pageX", "pageY")),
                  as.numeric),
           pageX = .data$pageX + .data$width / 2,
           pageY = .data$pageY + .data$height / 2) %>%
    filter(grepl("^(answer|java)", .data$target.id),
           .data$width > 0, .data$height > 0)
  if (!is.null(surveyStructure)) {
    logData <- logData %>%
      mutate(rowNumber = 1:n())
    arrayQuestions <- logData %>%
      inner_join(surveyStructure %>%
                   select(.data$target.id, .data$questionId,
                          .data$questionFormat, .data$subquestionCode,
                          .data$answerCode) %>%
                   filter(.data$questionFormat %in% c("A", "B", "C", "E",
                                                      "F", "H", ";", ";")),
                 by = "target.id") %>%
      mutate(pageXTemp = .data$pageX,
             pageY = ifelse(.data$questionFormat %in% "H",
                            .data$pageX, .data$pageY),
             pageX = ifelse(.data$questionFormat %in% "H",
                            .data$pageXTemp, .data$pageX)) %>%
      select(-.data$pageXTemp)
    correctionsX <- arrayQuestions %>%
      select({{respScreenIds}}, .data$questionId, .data$answerCode,
             .data$pageX) %>%
      distinct() %>%
      group_by(across({{respScreenIds}}), .data$questionId) %>%
      mutate(correctionX = .data$pageX - lag(.data$pageX)) %>%
      mutate(correctionX = 0.5 * c(.data$correctionX[-1],
                                   do.call(imputeLastPageXY,
                                           list(.data$correctionX[-1])))) %>%
      ungroup() %>%
      mutate(pageX = .data$pageX + ifelse(is.na(.data$correctionX),
                                          0, .data$correctionX)) %>%
      select(-.data$correctionX)
    correctionsY <- arrayQuestions %>%
      select({{respScreenIds}}, .data$questionId, .data$subquestionCode,
             .data$pageY) %>%
      distinct() %>%
      group_by(across({{respScreenIds}}), .data$questionId) %>%
      mutate(correctionY = .data$pageY - lag(.data$pageY)) %>%
      mutate(correctionY = 0.5 * c(.data$correctionY[-1],
                                   do.call(imputeLastPageXY,
                                           list(.data$correctionY[-1])))) %>%
      ungroup() %>%
      mutate(pageY = .data$pageY + ifelse(is.na(.data$correctionY),
                                          0, .data$correctionY)) %>%
      select(-.data$correctionY)
    arrayQuestions <- arrayQuestions %>%
      select(-.data$pageX, -.data$pageY) %>%
      left_join(correctionsX, by = setdiff(names(correctionsX), "pageX")) %>%
      left_join(correctionsY, by = setdiff(names(correctionsY), "pageY")) %>%
      mutate(pageXTemp = .data$pageX,
             pageY = ifelse(.data$questionFormat %in% "H",
                            .data$pageX, .data$pageY),
             pageX = ifelse(.data$questionFormat %in% "H",
                            .data$pageXTemp, .data$pageX)) %>%
      select(-.data$pageXTemp)
    logData <- bind_rows(logData %>%
                           anti_join(surveyStructure %>%
                                       select(.data$target.id, .data$questionId,
                                              .data$questionFormat,
                                              .data$subquestionCode,
                                              .data$answerCode) %>%
                                       filter(.data$questionFormat %in%
                                                c("A", "B", "C", "E",
                                                  "F", "H", ";", ";")),
                                     by = "target.id"),
                         arrayQuestions) %>%
      arrange(.data$rowNumber) %>%
      select(-.data$rowNumber)
  } else {
    warning("With no `surveyStructure` argument provided input positions reported for array type questions will not describe actual position of elements on the page.")
  }
  return(logData)
}
