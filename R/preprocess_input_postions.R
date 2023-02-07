#' @title Performing preprocessing of input positions
#' @description Function is called internally by
#' \code{\link{separate_logdata_types}} and is not exported (i.e. it is not
#' intended to be called by package users themselves). It selects records
#' describing input elements positions from a data frame storing different
#' types of log-data (returned by \code{\link{logstreams_to_data_frame}}) and
#' performs a preprocessing that includes:
#' \itemize{
#'   \item{Converting columns that are supposed to store numeric values to
#'         a numeric type}
#'   \item{Transforming \code{pageX} and \code{pageY} columns so they describe
#'         position of the \strong{center} of the element instead of position
#'         of its upper-left corner.}
#'   \item{If  \code{surveyStructure} argument is provided, transforming
#'         \code{pageX} and \code{pageY} columns \strong{for inputs from
#'         array-type questions} so they describe actual position of an element
#'         on the page (however, there are some limitations - see \emph{Details}
#'         section below).}
#' }
#' Last two transformations make returned data more useful while creating
#' backgrounds for plots showing cursor moves.
#' @inheritParams preprocess_actions
#' @param imputeLastPageXY A function that will be used to impute position of
#' inputs in last subquestions/answers in array-format questions - see
#' \emph{Details} section below. It is \code{\link{median}} by default.
#' @details Because of some aspects of how \emph{LimeSurvey's} responsive
#' layout works with respect to array type questions in questions of this
#' type(s) position of INPUT elements do not describe actual position at which
#' these elements were visible for the respondent (i.e. center of an array cell;
#' instead, these positions describe the upper-left corner of a table cell in
#' which a given element was placed). \strong{Function can identify array type
#' questions only if \code{respScreenIds} argument is provided} and then it
#' tries to correct reported positions so they describe actual position of INPUT
#' elements on the page. However, there are some limitations:
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
#'         same width in array type questions. \strong{But with respect to the
#'         vertical axis, it is nothing more than a lucky guess, however.}}
#'   \item{\strong{If there is only one answer or subquestion, position on
#'         a respective dimension can not be corrected (because of the lack of
#'         data) and it is therefore left unchanged.}}
#'   \item{At the moment \strong{no corrections are made for an array
#'         dual-scale questions}.}
#' }
#' @return A data frame with columns:
#' \describe{
#'   \item{respId}{Column(s) defined by \code{respId}.}
#'   \item{screenId}{Column(s) defined by \code{screenId}.}
#'   \item{target.tagName}{Type of an element.}
#'   \item{target.id}{Id of an element.}
#'   \item{target.class}{CSS classes assigned to an element.}
#'   \item{width}{Element width.}
#'   \item{height}{Element height.}
#'   \item{pageX}{Location of an element on the webpage - horizontal axis.}
#'   \item{pageY}{Location of an element on the webpage - vertical axis.}
#' }
#' Additionally, if \code{surveyStructure} argument was provided also with
#' columns:
#' \describe{
#'   \item{questionId}{Question id (in LimeSurvey database).}
#'   \item{questionFormat}{A letter code of a question format - see
#'                         \href{https://manual.limesurvey.org/Question_object_types#Current_question_types}{LimeSurvey documentation page}.}
#'   \item{questionCode}{Question code.}
#'   \item{subquestionCode}{Subquestion code.}
#'   \item{answerCode}{Answer code.}
#' }
#' @seealso \code{\link{preprocess_system_info}},
#' \code{\link{separate_logdata_types}}
#' @importFrom stats median
#' @importFrom dplyr %>% .data all_of anti_join filter inner_join lag lead
#' mutate n select
preprocess_input_positions <- function(logData, respId, screenId,
                                       surveyStructure = NULL,
                                       imputeLastPageXY = median) {
  logData <- logData %>%
    filter(.data$type %in% c("input_position")) %>%
    select({{respId}}, {{screenId}}, .data$target.tagName, .data$target.id,
           .data$target.class, width = .data$which, height = .data$metaKey,
           .data$pageX, .data$pageY)
  if (nrow(logData) == 0) return(NULL)
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
      mutate(rowNumber = 1:n()) %>%
      left_join(surveyStructure %>%
                  select(.data$target.id, .data$questionId, .data$questionCode,
                         .data$questionFormat, .data$subquestionCode,
                         .data$answerCode),
                by = "target.id")
    arrayQuestions <- logData %>%
      filter(.data$questionFormat %in% c("A", "B", "C", "E",
                                         "F", "H", ";", ";")) %>%
      mutate(pageXTemp = .data$pageX,
             pageY = ifelse(.data$questionFormat %in% "H",
                            .data$pageX, .data$pageY),
             pageX = ifelse(.data$questionFormat %in% "H",
                            .data$pageXTemp, .data$pageX)) %>%
      select(-.data$pageXTemp)
    correctionsX <- arrayQuestions %>%
      select({{respId}}, {{screenId}}, .data$questionId, .data$subquestionCode,
             .data$answerCode, .data$pageX) %>%
      distinct() %>%
      group_by(across(c({{respId}}, {{screenId}})), .data$questionId) %>%
      mutate(correctionX = lead(.data$pageX) - .data$pageX) %>%
      mutate(correctionX = ifelse(.data$correctionX < 0,
                                  NA_real_, .data$correctionX)) %>%
      mutate(correctionX =
               0.5 * ifelse(is.na(.data$correctionX),
                            do.call(imputeLastPageXY,
                                    list(.data$correctionX[!is.na(.data$correctionX)])),
                            .data$correctionX)) %>%
      ungroup() %>%
      mutate(pageX = .data$pageX + ifelse(is.na(.data$correctionX),
                                          0, .data$correctionX)) %>%
      select(-.data$correctionX)
    correctionsY <- arrayQuestions %>%
      select({{respId}}, {{screenId}}, .data$questionId, .data$subquestionCode,
             .data$pageY) %>%
      distinct() %>%
      group_by(across(c({{respId}}, {{screenId}})), .data$questionId) %>%
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
                           filter(!(.data$questionFormat %in%
                                      c("A", "B", "C", "E",
                                        "F", "H", ";", ";"))),
                         arrayQuestions) %>%
      arrange(.data$rowNumber) %>%
      select(-.data$rowNumber)
  } else {
    warning("With no `surveyStructure` argument provided input positions reported for array type questions will not describe actual position of elements on the page.")
  }
  return(logData)
}
#' @title Performing preprocessing of input positions
#' @description Function is called internally by
#' \code{\link{separate_logdata_types}} and is not exported (i.e. it is not
#' intended to be called by package users themselves). It calculates
#' \emph{relative} (see \code{\link{compute_relative_positions}}) positions
#' of the INPUT elements and adds this data to the input data.
#' @param inputPositions A data frame returned by
#' \code{\link{preprocess_input_positions}}
#' @param systemInfo A data frame returned by
#' \code{\link{preprocess_system_info}}
#' @param joinBy A character vector providing columns by which
#' \code{inputPositions} and \code{systemInfo} should be joined.
#' @return Input data frame with additional columns:
#' \describe{
#'   \item{width_rel}{\emph{Relative} width of an INPUT element.}
#'   \item{height_rel}{\emph{Relative} height of an INPUT element.}
#'   \item{pageX_rel}{\emph{Relative} position of an INPUT element on horizontal
#'                    axis.}
#'   \item{pageY_rel}{\emph{Relative} position of an INPUT element on vertical
#'                    axis.}
#' }
#' @seealso \code{\link{preprocess_input_positions}},
#' \code{\link{preprocess_system_info}}, \code{\link{separate_logdata_types}},
#' \code{\link{compute_relative_positions}}
#' @importFrom stats median
#' @importFrom dplyr %>% .data all_of left_join mutate select
compute_relative_input_positions <- function(inputPositions, systemInfo,
                                             joinBy) {
  if (is.null(inputPositions)) return(NULL)
  inputPositions %>%
    left_join(systemInfo %>%
                select(all_of(joinBy),
                       all_of(c("inputsMinPageX", "inputsMinPageY",
                                "inputsWidth", "inputsHeight"))),
              by = joinBy) %>%
    mutate(width_rel = .data$width / .data$inputsWidth,
           height_rel = .data$height / .data$inputsHeight,
           pageX_rel = (.data$pageX - .data$inputsMinPageX) / .data$inputsWidth,
           pageY_rel = (.data$pageY - .data$inputsMinPageY) / .data$inputsHeight) %>%
    select(-all_of(c("inputsMinPageX", "inputsMinPageY",
                     "inputsWidth", "inputsHeight"))) %>%
    return()
}
