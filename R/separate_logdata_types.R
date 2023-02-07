#' @title Separating raw log-data into information on system, input position and
#' actions
#' @description Function separates raw log-data streams into three separate data
#' frames containing:
#' \itemize{
#'   \item{\emph{System information}: data on respondent's browser, OS,
#'         screen and browser window resolution.}
#'   \item{Information about the \emph{layout} of the survey screen.
#'         Specifically, position of each INPUT element on the page. This
#'         information will be useful to standardize cursor moves indicators
#'         with respect to the differences in the survey page layout between
#'         respondents and also to draw backgrounds to graphs presenting
#'         cursor trajectories.}
#'   \item{Information about respondent's \emph{actions}.}
#' }
#' Data frame provided as an input typically comes from reading a CSV file with
#' survey results that was previously exported from the LimeSurvey. It should
#' contain \strong{only} 1) responses to the questions (columns) that were used
#' to store log-data streams and 2) columns explicitly stated in the
#' \code{respId} argument (that need not be only ids, actually).
#' @param logData A data frame with log-data streams recorded by the
#' \emph{logDataLimeSurvey} applet stored in its columns.
#' @param surveyStructure Optionally either a name (or vector of names) of a
#' \emph{LimeSurvey} survey structure file exported in the text (.txt) format or
#' a data frame with such a file already read, or list of data frames with such
#' files already read.
#' @param respId <\code{\link[dplyr:dplyr_tidy_select]{tidy-select}}>
#' Variable(s) identifying respondent. (You may also list additional variables
#' here, that you want to keep joined with your log-data).
#' @param questionNamesTo A string - name of the variable that will identify
#' survey screen (page) in the results (i.e. one storing names of variables
#' storing log-data streams in the input data).
#' @param questionNamesPrefix A string - prefix of names of variables storing
#' log-data streams that should be deleted while turning these names into values
#' of the variable identifying survey screen.
#' @param inputsBoxCells A logical value - whether \strong{in a case of
#' table-format questions} \code{inputsMinPageX}, \code{inputsMinPageY},
#' \code{inputsWidth} and \code{inputsHeight} should be computed with respect to
#' boundaries (vertexes) of table cells storing the input elements rather than
#' with respect to input element positions. Applied only for table-format
#' question that are the only questions on a given page.
#' @return A list of three data frames with elements named \emph{systemInfo},
#' \emph{inputPositions} and  \emph{actions}. More information on how these
#' data frames are constructed you may find in the documentation regarding
#' functions listed below in the \emph{See also} section.
#' @seealso \code{\link{read_survey_structure}}
#' \code{\link{logstreams_to_data_frame}}
#' \code{\link{preprocess_input_positions}} \code{\link{preprocess_system_info}}
#' \code{\link{find_problems}} \code{\link{preprocess_actions}}
#' \code{\link{separate_stagnations}}
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr %>% .data across all_of any_of distinct filter first
#' group_by last left_join mutate select ungroup
#' @export
separate_logdata_types <-
  function(logData, surveyStructure = NULL,
           respId = any_of(c("id", "token", "respid")),
           questionNamesTo = "screen", questionNamesPrefix = "",
           inputsBoxCells = FALSE) {
    stopifnot(is.data.frame(logData),
              is.character(questionNamesTo), length(questionNamesTo) == 1,
              is.character(questionNamesPrefix),
              length(questionNamesPrefix) == 1,
              is.logical(inputsBoxCells), length(inputsBoxCells) == 1,
              inputsBoxCells %in% c(TRUE, FALSE))
    emptyColumns <- sapply(logData, function(x) {return(all(is.na(x)))})
    emptyColumnNames <- names(logData)[emptyColumns]
    respIdColumns <- names(select(logData, {{respId}}))
    stopifnot("All columns in `logData` except these listed in `respId` must be character vectors." =
                all(sapply(logData, is.character) | emptyColumns |
                      names(logData) %in% respIdColumns))
    if (!is.null(surveyStructure)) {
      surveyStructure <- read_survey_structure(surveyStructure)
    }
    logData <- logData %>%
      select(-all_of(names(logData)[emptyColumns]))
    message("Processing log-data streams with:\n",
            "- respondent's id(s) stored in columns: ",
            paste(respIdColumns, collapse = ", "), ";\n",
            "- log-data streams stored in columns: ",
            paste(setdiff(names(logData), respIdColumns), collapse = ", "), ".")
    if (any(emptyColumns)) {
      message("\nColumns that were discarded because they contain only NAs:\n",
              paste(emptyColumnNames, collapse = ", "))
    }

    message("\nPreprocessing log-data streams:")
    logData <- logstreams_to_data_frame(logData, {{respId}}, questionNamesTo,
                                        questionNamesPrefix)
    message("Processing input positions.")
    inputPositions <- preprocess_input_positions(logData, {{respId}},
                                                 all_of(questionNamesTo),
                                                 surveyStructure)
    message("Processing system information.")
    systemInfo <- preprocess_system_info(logData, inputPositions, {{respId}},
                                         all_of(questionNamesTo), inputsBoxCells)

    inputPositions <- compute_relative_input_positions(inputPositions,
                                                       systemInfo,
                                                       c(respIdColumns,
                                                         questionNamesTo))
    message("Processing actions:")
    logData <- preprocess_actions(logData, {{respId}}, all_of(questionNamesTo),
                                  surveyStructure)
    systemInfo <- systemInfo %>%
      left_join(logData %>%
                  group_by(across(c({{respId}}, all_of(questionNamesTo)))) %>%
                  summarise(lastTimeStampRel = last(.data$timeStampRel),
                            .groups = "drop"),
                by = c(respIdColumns, questionNamesTo)) %>%
      left_join(find_problems(logData, systemInfo,
                              {{respId}}, all_of(questionNamesTo)),
                by = c(respIdColumns, questionNamesTo))

    message("\nSeparated data consists of:\n",
            "- ", systemInfo %>% select({{respId}}) %>% distinct() %>% nrow() %>%
              format(big.mark = "'"), " respondents on ",
            systemInfo %>% select(all_of(questionNamesTo)) %>% distinct() %>%
              nrow() %>% format(big.mark = "'"), " screens,\n",
            "- ", systemInfo %>% nrow() %>% format(big.mark = "'"),
            " respondent-screens,\n",
            "- ", nrow(logData) %>% format(big.mark = "'"), " actions,\n",
            "  out of which data about ", sum(logData$broken) %>%
              format(big.mark = "'"),
            " is somehow broken.")

    return(list(systemInfo = systemInfo,
                inputPositions = inputPositions,
                actions = logData))
}
