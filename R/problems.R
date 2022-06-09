#' @title Marking problems in data regarding events
#' @description Function is called internally by
#' \code{\link{separate_logdata_types}} and is not exported (i.e. it is not
#' intended to be called by package users themselves). It identifies possible
#' problems in log-data and reports for which respondent-screens they occur.
#' @inheritParams compute_aat
#' @return A data frame with columns:
#' \describe{
#'   \item{respId}{Column(s) defined by \code{respId}.}
#'   \item{screenId}{Column(s) defined by \code{screenId}.}
#'   \item{problemsLeftBrowser}{Whether respondent left browser window (card)
#'                              while answering a given screen,}
#'   \item{problemsResized}{Whether respondent changed the size of a browser
#'                          window while answering a given screen,}
#'   \item{problemsNoPageLoaded}{Whether log-data for a given respondent-screen
#'                               does not contain a \emph{pageLoaded} event (if
#'                               so, it is most probably because an old version
#'                               of the JavaScript applet - that not recorded
#'                               this type of events - was used to collect the
#'                               log-data),}
#'   \item{problemsTimeStamps}{Whether there is a discontinuity in values of
#'                            time-stamps for a given respondent-screen that
#'                            results in a negative duration of some
#'                            \emph{mousemove} events (if so, it is most
#'                            probably because an old version of the JavaScript
#'                            applet - that used an unreliable method of getting
#'                            time-stamps - was used to collect the log-data).}
#' }
#' @importFrom dplyr %>% .data across group_by summarise
find_problems <- function(actions, respId, screenId) {
  actions %>%
    group_by(across(c({{respId}}, {{screenId}}))) %>%
    summarise(problemsAnyBroken = as.integer(any(.data$broken %in% 1)),
              problemsLeftBrowser = as.integer(any(.data$type %in% "blur")),
              problemsResized = as.integer(any(.data$type %in% "resize")),
              problemsNoPageLoaded = as.integer(!any(.data$type %in% "pageLoaded")),
              problemsTimeStamps = as.integer(any(.data$duration < 0 & !is.na(.data$duration))),
              .groups = "drop") %>%
    return()
}
#' @title Removing problems identified while separating log-data
#' @description Function enables to remove from log-data object prepared by
#' \code{\link{separate_logdata_types}} those records that were identified
#' (by this function) to be somehow problematic. Function works interactively
#' presenting user description of the problem and number of records affected
#' and enables to choose whether given records should be removed or kept.
#' @param x Either a list returned by \code{\link{separate_logdata_types}} or
#' element \emph{actions} of such a list. In the latter case also argument
#' \code{systemInfo} must be provided.
#' @inheritParams compute_cursor_indices
#' @details Every column which name starts with \emph{problems} in the
#' \emph{systemInfo} element of \code{x} or in \code{systemInfo} argument will
#' be treated as marking problems in the data.
#' @return Input data (either a data frame or a list of three data frames)
#' possibly with some rows removed.
#' @seealso \code{\link{separate_logdata_types}},
#' \code{\link{separate_stagnations}}
#' @importFrom dplyr %>% anti_join cur_data filter select semi_join
#' @importFrom utils menu
#' @export
remove_problems <- function(x, systemInfo = NULL,
                            respId = any_of(c("id", "token", "respid")),
                            screenId = all_of("screen")) {
  if (!is.null(systemInfo)) {
    stopifnot(is.data.frame(x),
              is.data.frame(systemInfo))
    respIdColumns <- names(select(systemInfo, {{respId}}))
    screenIdColumns <- names(select(systemInfo, {{screenId}}))
    stopifnot(all(respIdColumns %in% names(x)),
              all(screenIdColumns %in% names(x)))
    x <- list(actions = x,
              systemInfo = systemInfo)
  } else {
    stopifnot(is.list(x), length(x) == 3,
              all(c("systemInfo", "inputPositions", "actions") %in% names(x)))
    stopifnot(is.data.frame(x$systemInfo),
              is.data.frame(x$inputPositions),
              is.data.frame(x$actions))
    respIdColumns <- names(select(x$systemInfo, {{respId}}))
    screenIdColumns <- names(select(x$systemInfo, {{screenId}}))
    stopifnot(all(respIdColumns %in% names(x$actions)),
              all(respIdColumns %in% names(x$inputPositions)),
              all(screenIdColumns %in% names(x$actions)),
              all(screenIdColumns %in% names(x$inputPositions)))
  }
  duplicates <- x$systemInfo %>%
    select({{respId}}, {{screenId}}) %>%
    filter(duplicated(cur_data()))
  if (nrow(duplicates) > 0) {
    w <- menu(c("To stop the procedure (throwing an error) not modyfing the data",
                "To remove all these respondent-screens from the data and proceed further"),
              title = paste0("In the data identyfing problems there are ",
                             format(nrow(distinct(duplicates)), big.mark = "'"),
                             " respondent-screens that have their duplicates",
                             " (there are ",
                             format(nrow(duplicates), big.mark = "'"),
                             " duplicated rows in all), given respondent id is/are: '",
                             paste0(respIdColumns, collapse = "', '"),
                             "' and screen id is/are: '",
                             paste0(screenIdColumns, collapse = "', '"),
                             "'.\n\nDo you want:"))
    if (w %in% c(0L, 1L)) {
      stop("Duplicated respondent-screens in the input data identyfing problems.",
           call. = FALSE)
    } else {
      x$systemInfo <- x$systemInfo %>%
        anti_join(duplicates, by = c(respIdColumns, screenIdColumns))
      n <- nrow(x$actions)
      x$actions <- x$actions %>%
        anti_join(duplicates, by = c(respIdColumns, screenIdColumns))
      message("Accordingly ", format(n - nrow(x$actions), big.mark = "'"),
              " records were removed from the data regarding actions (events).")
      if ("inputPositions" %in% names(x)) {
        n <- nrow(x$inputPositions)
        x$inputPositions <- x$inputPositions %>%
          anti_join(duplicates, by = c(respIdColumns, screenIdColumns))
        message("Accordingly ", format(n - nrow(x$inputPositions), big.mark = "'"),
                " records were removed from the data regarding INPUT positions.")
      }
    }
  }

  # Checking whether systemInfo matches actions (and the other way round) ######
  nMissing <- nrow(anti_join(x$systemInfo, x$actions,
                             by = c(respIdColumns, screenIdColumns)))
  if (nMissing > 0) {
    w <- menu(c("Keep them", "Remove them"),
              title = paste0("There are ", format(nMissing, big.mark = "'"),
                             " records (respondent-screens) in the data identyfing problems that can not me matched in the data regarding actions (events).\n",
                             "This may be due to respondents quiting the survey just at the beginning of a given screen, but it is also possible that the data regarding actions has been somehowew filtered out already. Anyway you may remove them, because no indices can be computed for such respondent-screens.\n\n",
                             "What do you want to do with these respondent-screens?"))
    if (w == 0L) {
      stop("Procedure aborted.", call. = FALSE)
    } else if (w == 2L) {
      x$systemInfo <- semi_join(x$systemInfo, x$actions,
                                by = c(respIdColumns, screenIdColumns))
    }
  }
  nMissing <- nrow(anti_join(x$actions, x$systemInfo,
                             by = c(respIdColumns, screenIdColumns)))
  if (nMissing > 0) {
    w <- menu(c("Keep them", "Remove them"),
              title = paste0("There are ", format(nMissing, big.mark = "'"),
                             " records (rows) in the data regarding actions",
                             " (events) that can not be matched to the data",
                             " (respondent-screens) identyfing problems.\n\n",
                             "What do you want to do with these records?"))
  } else {
    w <- 1L
  }
  if (w == 0L) {
    stop("Procedure aborted.", call. = FALSE)
  } else if (w == 2L) {
    x$actions <- x$actions %>%
      semi_join(x$systemInfo, by = c(respIdColumns, screenIdColumns))
    if ("inputPositions" %in% names(x)) {
      x$inputPositions <- x$inputPositions %>%
        semi_join(x$systemInfo, by = c(respIdColumns, screenIdColumns))
    }
  }

  # Handling problems ##########################################################
  problems <- grep("^problems", names(x$systemInfo), value = TRUE )
  removeProblems <- rep(TRUE, length(problems))
  names(removeProblems) <- problems
  if (length(problems) == 0L) {
    message("No columns identyfing problems has been found.")
    return(x)
  }
  problems <- problems[sapply(x$systemInfo[problems],
                              function(x) {return(any(x %in% 1))})]
  if (length(problems) == 0L) {
    message("No problems has been found in the data.")
    return(x)
  }
  for (i in problems) {
    currentProblem <- x$systemInfo %>%
      filter(.data[[i]] %in% 1) %>%
      select({{respId}}, {{screenId}})
    n <- x$actions %>%
      semi_join(currentProblem, by = c(respIdColumns, screenIdColumns)) %>%
      nrow()
    w <- menu(c("Keep them ", "Remove them"),
              title = paste0("There are ",
                             format(nrow(currentProblem), big.mark = "'"), " (",
                             round(100*nrow(currentProblem) / nrow(x$systemInfo), 2),
                             "%) respondent-screens matching ",
                             format(n, big.mark = "'"),
                             " (", round(100*n / nrow(x$actions), 2),
                             "%) records in the data regarding actions (events)",
                             switch(i,
                                    problemsAnyBroken = " that contain some broken records regarding actions.\n\nTypically you should remove such respondent-screens.",
                                    problemsLeftBrowser = " during which respondent has left the browser window (tab) presenting the survey while answering this screen.\n\nYou may keep such respondent-screens if you are interested only in computing number of edits. Think carefully what to do if you are going to compute hovering indices or answering time indices. You should exclude such respondent-screens if you are interested in cursor moves indices.",
                                    problemsResized = " during which respondent resized the browser window.\n\nYou should rather remove such repondent-screens if you are going to compute cursor moves indices. If you want to compute indices regarding timing or number of edits, you may keep them. Decide carefully what to do if you are interested in hovering times (you can compute hovering indices for such respondent-screen but it will be unreasonable to plot for example a heat map for such a respondent-screen).",
                                    problemsNoPageLoaded = " that do not contain a 'pageLoaded' event (this probably means that an older version of the 'logdataLimeSurvey' JavaScript applet was used to collect the log-data).\n\nTypically you may keep such respondent-screens with no harm.",
                                    problemsTimeStamps = " that contain inconsistent time-stamps (this probably stems from the prompt regarding unanswered questions or wrong response format was shown to the respondent while he/she attempted to proceed to the next screen while an older version of the 'logdataLimeSurvey' JavaScript applet was used to collect the log-data).\n\nYou should remove these respondent-screens if you are going to compute any indicators that rely on timing of events (i.e. virtually all with the exception of number of edits).",
                                    paste0("that are identified as including problems by the variable '",
                                           i, "'.")),
                             "\n\nWhat do you want to do with these records?"))
    if (w == 0L) {
      stop("Procedure aborted.", call. = FALSE)
    } else if (w == 1L) {
      removeProblems[i] <- FALSE
    }
  }
  mRemoved <- nrow(x$systemInfo)
  nRemoved <- nrow(x$actions)
  problems <- problems[removeProblems]
  x$systemInfo <- x$systemInfo %>%
    filter((cur_data() %>%
              select(all_of(problems)) %>%
              rowSums()) == 0)
  mRemoved <- mRemoved - nrow(x$systemInfo)
  x$actions <- x$actions %>%
    semi_join(x$systemInfo, by = c(respIdColumns, screenIdColumns))
  nRemoved <- nRemoved - nrow(x$actions)
  if ("inputPositions" %in% names(x)) {
    iRemoved <- nrow(x$inputPositions)
    x$inputPositions <- x$inputPositions %>%
      semi_join(x$systemInfo, by = c(respIdColumns, screenIdColumns))
    iRemoved <- iRemoved - nrow(x$inputPositions)
    iRemoved <- paste0(",\n- ", format(iRemoved, big.mark = "'"), " (",
                       round(100*iRemoved / (iRemoved + nrow(x$inputPositions)), 2),
                       "%) INPUT positions.")
  } else {
    iRemoved <- NA_character_
  }
  message("Given all the above choices there were removed:\n- ",
          format(mRemoved, big.mark = "'"), " (",
          round(100*mRemoved / (mRemoved + nrow(x$systemInfo)), 2),
          "%) respondent-screens,\n- ",
          format(nRemoved, big.mark = "'"), " (",
          round(100*nRemoved / (nRemoved + nrow(x$actions)), 2),
          "%) actions (events)",
          ifelse(is.na(iRemoved), ".", iRemoved))

  if (!is.null(systemInfo)) {
    return(x$actions)
  } else {
    return(x)
  }
}
