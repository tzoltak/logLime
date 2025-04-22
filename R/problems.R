#' @title Marking problems in data regarding events
#' @description Function is called internally by[separate_logdata_types] and
#' is not exported (i.e. it is not intended to be called by package users
#' themselves). It identifies possible problems in log-data and reports for
#' which respondent-screens they occur.
#' @param actions A data frame containing data regarding *actions* -
#' typically element *actions* of the list returned by [separate_logdata_types].
#' @inheritParams remove_problems
#' @return A data frame with columns:
#' \describe{
#'   \item{respId}{Column(s) defined by `respId`.}
#'   \item{screenId}{Column(s) defined by `screenId`.}
#'   \item{problemsAnyBroken}{Whether a given respondent-screen contains any
#'                            broken records,}
#'   \item{problemsLeftBrowser}{Whether respondent left browser window (card)
#'                              while answering a given screen,}
#'   \item{problemsResized}{Whether respondent changed the size of a browser
#'                          window while answering a given screen,}
#'   \item{problemsNoPageLoaded}{Whether log-data for a given respondent-screen
#'                               does not contain a *pageLoaded* event (if
#'                               so, it is most probably because an old version
#'                               of the JavaScript applet - that not recorded
#'                               this type of events - was used to collect the
#'                               log-data),}
#'   \item{problemsNoSubmit}{Whether log-data for a given respondent-screen
#'                           does not contain a *submit* event (if so, it
#'                           is most probably because an old version of the
#'                           JavaScript applet - that not recorded this type of
#'                           events - was used to collect the log-data),}
#'   \item{problemsTimeStamps}{Whether there is a discontinuity in values of
#'                            time-stamps for a given respondent-screen that
#'                            results in a negative duration of some
#'                            *mousemove* events (if so, it is most
#'                            probably because an old version of the JavaScript
#'                            applet - that used an unreliable method of getting
#'                            time-stamps - was used to collect the log-data),}
#'   \item{problemsNoActions}{Whether there are no *actions* at all in the
#'                            recorded log-data for a given respondent-screen.}
#' }
#' @importFrom dplyr %>% .data across group_by summarise
find_problems <- function(actions, systemInfo, respId, screenId, entryId) {
  systemInfo <- systemInfo %>%
    select({{respId}}, {{screenId}}, {{entryId}})
  actions %>%
    group_by(across(c({{respId}}, {{screenId}}, {{entryId}}))) %>%
    summarise(problemsAnyBroken = as.integer(any(.data$broken %in% 1)),
              problemsLeftBrowser = as.integer(any(.data$type %in% "blur")),
              problemsResized = as.integer(any(.data$type %in% "resize")),
              problemsNoPageLoaded = as.integer(!any(.data$type %in% "pageLoaded")),
              problemsNoSubmit = as.integer(!any(.data$type %in% "submit") |
                                              sum(.data$type %in% "submit") !=
                                              sum(.data$type %in% "pageLoaded")),
              problemsTimeStamps = as.integer(any(.data$duration < 0 & !is.na(.data$duration)) |
                                                any(.data$timeStampRel < 0 | is.na(.data$timeStampRel))),
              .groups = "drop") %>%
    bind_rows(systemInfo %>%
                anti_join(actions, by = names(systemInfo)) %>%
                mutate(problemsNoActions = 1L)) %>%
    mutate(across(starts_with("problems"), ~ifelse(is.na(.), 0L, .))) %>%
    return()
}
#' @title Removing problems identified while separating log-data
#' @description Function enables to remove from log-data object prepared by
#' [separate_logdata_types] those records that were identified
#' (by this function) to be somehow problematic.
#' **Function works interactively** presenting user description of the problem
#' and number of records affected and enables to choose whether given records
#' should be removed or kept.
#' @param x Either a list returned by [separate_logdata_types] or element
#' *actions* of such a list. In the latter case also argument `systemInfo`
#' must be provided.
#' @inheritParams compute_cursor_indices
#' @param systemInfo A data frame containing *system information* - typically
#' element *systemInfo* of the list returned by [separate_logdata_types].
#' @param level A string indicating at which *level* records containing problems
#' should be excluded - a screen or an *entry* (i.e. visit to a screen). It is
#' applicable only if columns identified by `entryId` appear in the input data;
#' **otherwise screen level will be set automatically**.
#' @details Every column which name starts with *problems* in the *systemInfo*
#' element of `x` or in `systemInfo` argument will be treated as marking
#' problems in the data.
#' @return Input data (either a data frame or a list of three data frames)
#' possibly with some rows removed.
#' @seealso [separate_logdata_types], [separate_returns], [separate_stagnations]
#' @importFrom dplyr %>% anti_join everything filter n n_distinct pick select
#' semi_join
#' @importFrom utils menu
#' @export
remove_problems <- function(x, systemInfo = NULL,
                            respId = any_of(c("id", "token", "respid")),
                            screenId = "screen",
                            entryId = any_of("entry"),
                            level = c("screen", "entry")) {
  level <- match.arg(level)
  if (!is.null(systemInfo)) {
    stopifnot(is.data.frame(x),
              is.data.frame(systemInfo))
    respIdColumns <- names(select(systemInfo, {{respId}}))
    screenIdColumns <- names(select(x$systemInfo, {{screenId}}))
    entryIdColumns <- names(select(x$systemInfo, {{entryId}}))
    stopifnot(all(respIdColumns %in% names(x)),
              all(screenIdColumns %in% names(x)),
              all(entryIdColumns %in% names(x)))
    x <- list(actions = x,
              systemInfo = systemInfo)
  } else {
    stopifnot(is.list(x), length(x) >= 3,
              all(c("systemInfo", "inputPositions", "actions") %in% names(x)))
    stopifnot(is.data.frame(x$systemInfo),
              is.data.frame(x$inputPositions),
              is.data.frame(x$actions))
    otherElementsNames <- setdiff(names(x),
                                  c("systemInfo", "inputPositions", "actions"))
    respIdColumns <- names(select(x$systemInfo, {{respId}}))
    screenIdColumns <- names(select(x$systemInfo, {{screenId}}))
    entryIdColumns <- names(select(x$systemInfo, {{entryId}}))
    stopifnot(all(respIdColumns %in% names(x$actions)),
              all(respIdColumns %in% names(x$inputPositions)),
              all(screenIdColumns %in% names(x$actions)),
              all(screenIdColumns %in% names(x$inputPositions)),
              all(entryIdColumns %in% names(x$actions)))
  }
  entries <- length(entryIdColumns) > 0L
  if (level == "screen" && entries) {
    x$systemInfo <- x$systemInfo %>%
      group_by(across(c({{respId}}, {{screenId}}))) %>%
      mutate(problemsResized = ifelse(rep(n_distinct(.data$browserWidth) > 1 ||
                                            n_distinct(.data$browserHeight) > 1 ||
                                            n_distinct(.data$screenWidth) > 1 ||
                                            n_distinct(.data$screenWidth) > 1,
                                          n()),
                                      rep(1L, n()), .data$problemsResized)) %>%
      summarise(across(starts_with("problems"), ~as.integer(sign(sum(.)))))
    entryId <- entryIdColumns <- vector(mode = "character", length = 0L)
    entries <- FALSE
  } else if (level == "entry") {
    stopifnot("There is/are no column(s) identifing entries in the data frame with system information." = entries)
  }
  duplicates <- x$systemInfo %>%
    select({{respId}}, {{screenId}}, {{entryId}}) %>%
    filter(duplicated(pick(everything())))
  if (nrow(duplicates) > 0) {
    w <- menu(c("To stop the procedure (throwing an error) not modyfing the data",
                paste0("To remove all these respondent-screen",
                       ifelse(entries, "-entrie", ""), "s from the data and proceed further")),
              title = paste0("In the data identyfing problems there are ",
                             format(nrow(distinct(duplicates)), big.mark = "'"),
                             " respondent-screen", ifelse(entries, "-entrie", ""),
                             "s that have their duplicates",
                             " (there are ",
                             format(nrow(duplicates), big.mark = "'"),
                             " duplicated rows), given respondent id is/are: '",
                             paste0(respIdColumns, collapse = "', '"),
                             "' and screen id is/are: '",
                             paste0(screenIdColumns, collapse = "', '"),
                             ifelse(entries,
                                    paste0("' and entry id is/are: '",
                                           paste0(entryIdColumns, collapse = "', '")),
                                    ""),
                             "'.\n\nDo you want:"))
    if (w %in% c(0L, 1L)) {
      stop("Duplicated respondent-screens in the input data identyfing problems.",
           call. = FALSE)
    } else {
      x$systemInfo <- x$systemInfo %>%
        anti_join(duplicates,
                  by = c(respIdColumns, screenIdColumns, entryIdColumns))
      n <- nrow(x$actions)
      x$actions <- x$actions %>%
        anti_join(duplicates,
                  by = c(respIdColumns, screenIdColumns, entryIdColumns))
      message("Accordingly ", format(n - nrow(x$actions), big.mark = "'"),
              " records were removed from the data regarding actions (events).")
      if ("inputPositions" %in% names(x)) {
        n <- nrow(x$inputPositions)
        x$inputPositions <- x$inputPositions %>%
          anti_join(duplicates,
                    by = c(respIdColumns, screenIdColumns))
        message("Accordingly ", format(n - nrow(x$inputPositions), big.mark = "'"),
                " records were removed from the data regarding INPUT positions.")
      }
    }
  }
  # Checking whether systemInfo matches actions (and the other way round) ######
  nMissing <- nrow(anti_join(x$systemInfo, x$actions,
                             by = c(respIdColumns, screenIdColumns,
                                    entryIdColumns)))
  if (nMissing > 0) {
    w <- menu(c("Keep them", "Remove them"),
              title = paste0("There are ", format(nMissing, big.mark = "'"),
                             " records (respondent-screen",
                             ifelse(entries, "-entrie", ""),
                             "s) in the data identyfing problems that do not match the data regarding actions (events).\n",
                             "This may be due to respondents quiting the survey just at the beginning of a given screen, but it is also possible that the data regarding actions has been somehow filtered out already. Anyway you may remove them, because no indices can be computed for such respondent-screens.\n\n",
                             "What do you want to do with these respondent-screen",
                             ifelse(entries, "-entrie", ""), "s?"))
    if (w == 0L) {
      stop("Procedure aborted.", call. = FALSE)
    } else if (w == 2L) {
      x$systemInfo <- semi_join(x$systemInfo, x$actions,
                                by = c(respIdColumns, screenIdColumns,
                                       entryIdColumns))
    }
  }
  nMissing <- nrow(anti_join(x$actions, x$systemInfo,
                             by = c(respIdColumns, screenIdColumns,
                                    entryIdColumns)))
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
      semi_join(x$systemInfo,
                by = c(respIdColumns, screenIdColumns, entryIdColumns))
    if ("inputPositions" %in% names(x)) {
      x$inputPositions <- x$inputPositions %>%
        semi_join(x$systemInfo,
                  by = c(respIdColumns, screenIdColumns))
    }
  }
  # Handling problems ##########################################################
  problems <- grep("^problems", names(x$systemInfo), value = TRUE )
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
  removeProblems <- rep(TRUE, length(problems))
  names(removeProblems) <- problems
  for (i in problems) {
    currentProblem <- x$systemInfo %>%
      filter(.data[[i]] %in% 1) %>%
      select({{respId}}, {{screenId}}, {{entryId}})
    n <- x$actions %>%
      semi_join(currentProblem,
                by = c(respIdColumns, screenIdColumns, entryIdColumns)) %>%
      nrow()
    w <- menu(c("Keep them ", "Remove them"),
              title = paste0("There are ",
                             format(nrow(currentProblem), big.mark = "'"), " (",
                             round(100*nrow(currentProblem) / nrow(x$systemInfo), 2),
                             "%) respondent-screen",
                             ifelse(entries, "-entrie", ""), "s matching ",
                             format(n, big.mark = "'"),
                             " (", round(100*n / nrow(x$actions), 2),
                             "%) records in the data regarding actions (events)",
                             switch(i,
                                    problemsAnyBroken = " which contain some broken records regarding actions.\n\nTypically you should remove such respondent-screens.",
                                    problemsLeftBrowser = " during which respondent has left the browser window (tab) presenting the survey while answering this screen.\n\nYou may keep such respondent-screens if you are interested only in computing number of edits. Think carefully what to do if you are going to compute hovering indices or answering time indices. You should exclude such respondent-screens if you are interested in cursor moves indices.",
                                    problemsResized = " during which respondent resized the browser window.\n\nYou should rather remove such repondent-screens if you are going to compute cursor moves indices. If you want to compute indices regarding timing or number of edits, you may keep them. Decide carefully what to do if you are interested in hovering times (you can compute hovering indices for such respondent-screen but it will be unreasonable to plot for example a heat map for such a respondent-screen).",
                                    problemsNoPageLoaded = " which do not contain a 'pageLoaded' event (this probably means that an older version of the 'logdataLimeSurvey' JavaScript applet was used to collect the log-data).\n\nTypically you may keep such respondent-screens with no harm.",
                                    problemsNoSubmit = " which do not contain a 'submit' event (this probably means that log-data collection stopped because its time limit was exceeded).\n\nTypically you should exclude such records because they are incomplete and will distort results.\n\n(If the problem occurs for all the respondent-screens, it probably means that an older version of the 'logdataLimeSurvey' JavaScript applet was used to collect the log-data, that did not collected 'submit' events. In that case, you should keep these records, but be aware that you should check for exceeding log-data collection time limit some other way.)",
                                    problemsTimeStamps = " which contain inconsistent time-stamps (this probably stems from the prompt regarding unanswered questions or wrong response format was shown to the respondent while he/she attempted to proceed to the next screen while an older version of the 'logdataLimeSurvey' JavaScript applet was used to collect the log-data).\n\nYou should remove these respondent-screens if you are going to compute any indicators that rely on timing of events (i.e. virtually all with the exception of number of edits).",
                                    problemsNoActions = " which contain not a single action recorded.",
                                    paste0("which are identified as including problems by the variable '",
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
    filter(rowSums(pick(all_of(problems))) == 0)
  mRemoved <- mRemoved - nrow(x$systemInfo)
  x$actions <- x$actions %>%
    semi_join(x$systemInfo,
              by = c(respIdColumns, screenIdColumns, entryIdColumns))
  nRemoved <- nRemoved - nrow(x$actions)
  if ("inputPositions" %in% names(x)) {
    iRemoved <- nrow(x$inputPositions)
    x$inputPositions <- x$inputPositions %>%
      semi_join(x$systemInfo,
                by = c(respIdColumns, screenIdColumns))
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
          "%) respondent-screen", ifelse(entries, "-entrie", ""), "s,\n- ",
          format(nRemoved, big.mark = "'"), " (",
          round(100*nRemoved / (nRemoved + nrow(x$actions)), 2),
          "%) actions (events)",
          ifelse(is.na(iRemoved), ".", iRemoved))

  if (!is.null(systemInfo)) {
    return(x$actions)
  } else {
    if (length(x) > 3) {
      warning("No data have been removed from the element(s): '",
              paste(otherElementsNames, collapse = "', '"),
              "' of the input list.")
    }
    return(x)
  }
}
