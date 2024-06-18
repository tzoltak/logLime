#' @title Separating raw log-data into information on system, input position and
#' actions
#' @description Function separates raw log-data streams into three separate data
#' frames containing:
#' \itemize{
#'   \item{*System information*: data on respondent's browser, OS,
#'         screen and browser window resolution.}
#'   \item{Information about the *layout* of the survey screen.
#'         Specifically, position of each INPUT element on the page. This
#'         information will be useful to standardize cursor moves indicators
#'         with respect to the differences in the survey page layout between
#'         respondents and also to draw backgrounds to graphs presenting
#'         cursor trajectories.}
#'   \item{Information about respondent's *actions*.}
#' }
#' Data frame provided as an input typically comes from reading a CSV file with
#' survey results that was previously exported from the LimeSurvey. It should
#' contain **only** 1) responses to the questions (columns) that were used
#' to store log-data streams and 2) columns explicitly stated in the
#' `respId` argument (that need not be only ids, actually).
#' @param logData A data frame with log-data streams recorded by the
#' *logDataLimeSurvey* applet stored in its columns.
#' @param surveyStructure Optionally either a name (or vector of names) of a
#' *LimeSurvey* survey structure file exported in the text (.txt) format or
#' a data frame with such a file already read, or list of data frames with such
#' files already read.
#' @param respId <[tidy-select][dplyr::dplyr_tidy_select]> Variable(s)
#' identifying respondent. (You may also list additional variables
#' here, that you want to keep joined with your log-data).
#' @param questionNamesTo A string - name of the variable that will identify
#' survey screen (page) in the results (i.e. one storing names of variables
#' storing log-data streams in the input data).
#' @param questionNamesPrefix A string - prefix of names of variables storing
#' log-data streams that should be deleted while turning these names into values
#' of the variable identifying survey screen.
#' @param inputsBoxCells A logical value - whether **in a case of table-format
#' questions** `inputsMinPageX`, `inputsMinPageY`, `inputsWidth` and
#' `inputsHeight` should be computed with respect to
#' boundaries (vertexes) of table cells storing the input elements rather than
#' with respect to input element positions. Applied only for table-format
#' question that are the only questions on a given page.
#' @param separateReturns A logical value indicating whether returns to the same
#' survey screen (different survey screen *entries*) should be identified by an
#' additional id-column in the returned results.
#' @param screenReturnThreshold (Applicable only if `separateReturns != "no"`)
#' The smallest number of milliseconds between the *pageLoaded* event directly
#' following the *submit* event that will be interpreted as indicating returning
#' to a given survey screen after visiting some other screen. If set to `Inf`,
#' returns will be identified only by looking on the sequence of screen visits
#' (this have limitations if paradata is not collected on some survey screens)
#' - see *Details* in [separate_returns].
#' @return A list of three data frames with elements named *systemInfo*,
#' *inputPositions* and  *actions*. More information on how these
#' data frames are constructed you may find in the documentation regarding
#' functions listed below in the *See also* section.
#' @seealso [read_survey_structure], [logstreams_to_data_frame],
#' [separate_returns], [preprocess_input_positions], [preprocess_system_info],
#' [preprocess_actions], [find_problems], [separate_stagnations]
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr %>% .data across all_of any_of distinct filter first
#' group_by last left_join mutate select ungroup
#' @export
separate_logdata_types <-
  function(logData, surveyStructure = NULL,
           respId = any_of(c("id", "token", "respid")),
           questionNamesTo = "screen", questionNamesPrefix = "",
           inputsBoxCells = FALSE,
           separateReturns = TRUE,
           screenReturnThreshold = 1000) {
    stopifnot(is.data.frame(logData),
              is.character(questionNamesTo), length(questionNamesTo) == 1L,
              !is.na(questionNamesTo),
              is.character(questionNamesPrefix),
              length(questionNamesPrefix) == 1, !is.na(questionNamesPrefix),
              is.logical(inputsBoxCells), length(inputsBoxCells) == 1L,
              inputsBoxCells %in% c(TRUE, FALSE),
              is.logical(separateReturns), length(separateReturns) == 1L,
              separateReturns %in% c(TRUE, FALSE),
              is.numeric(screenReturnThreshold),
              length(screenReturnThreshold) == 1L,
              !is.na(screenReturnThreshold))
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
    message("Separating returns to survey screens.")
    if (!all(c("pageLoaded", "submit") %in% logData$type) && separateReturns) {
      separateReturns <- FALSE
      warning("Separating returns to survey screens is not possible for log-data collected using 'logdataLimeSurvey' applet in versions earlier than 1.1. Argument `separateReturns` was set to 'no' automatically.")
    }
    if (separateReturns) {
      logData <- separate_returns(logData, {{respId}}, all_of(questionNamesTo),
                                  screenReturnThreshold)
    } else {
      logData <- logData %>%
        mutate(entry = 1L)
    }
    message("Processing input positions.")
    inputPositions <- preprocess_input_positions(logData, {{respId}},
                                                 all_of(questionNamesTo),
                                                 surveyStructure)
    otherInformation <- logData %>%
      filter(.data$timeStamp == -1,
             !(.data$type %in% c("browser", "screen", "input_position")))
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
                  group_by(across(c({{respId}}, all_of(questionNamesTo),
                                    "entry"))) %>%
                  summarise(lastTimeStampRel = last(.data$timeStampRel),
                            .groups = "drop"),
                by = c(respIdColumns, questionNamesTo, "entry")) %>%
      left_join(find_problems(logData, systemInfo,
                              {{respId}}, all_of(questionNamesTo), "entry"),
                by = c(respIdColumns, questionNamesTo, "entry"))

    message("\nSeparated data consists of:\n",
            "- ", systemInfo %>% select({{respId}}) %>% distinct() %>% nrow() %>%
              format(big.mark = "'"), " respondents on ",
            systemInfo %>% select(all_of(questionNamesTo)) %>% distinct() %>%
              nrow() %>% format(big.mark = "'"), " screens,\n",
            "- ", systemInfo %>% nrow() %>% format(big.mark = "'"),
            " respondent-screen-entries,\n",
            "- ", nrow(logData) %>% format(big.mark = "'"), " actions,\n",
            "  out of which data about ", sum(logData$broken) %>%
              format(big.mark = "'"),
            " is somehow broken.")
    if (!separateReturns) {
      systemInfo = systemInfo %>%
        filter(.data$entry == 1L) %>%
        select(-"entry")
      logData = logData %>% select(-"entry")
    } else {
      message("\nPlease note that the input positions were recorded only at the moment of a given respondent enetering a given screen for the first time. To protect against issued related with changing browser window size between different survey screen entries, use `remove_problems()` with argument `level' set to 'screen'.")
    }

    results <- list(systemInfo = systemInfo,
                    inputPositions = inputPositions,
                    actions = logData)
    if (nrow(otherInformation) > 0L) {
      results$otherInformation <- otherInformation
    }
    return(results)
}
