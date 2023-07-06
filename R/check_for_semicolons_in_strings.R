#' @title Handling historical difficulties
#' @description Function is called internally by [logstreams_to_data_frame] and
#' is not exported (i.e. it is not intended to be called by package users
#' themselves). It checks whether there are some additional semicolons in the
#' logstreams - if so, some of them don't mark data fields boundaries.
#' In a case there are such semicolons it converts the ones that occur between
#' single quota signs (') to colons. This enables to handle the problem with
#' very early versions of the *logdataLimeSurvey* applet that did not convert
#' semicolons in the `navigator.userAgent` to colons by itself, what further
#' caused problems while separating data into columns by
#' [logstreams_to_data_frame].
#' @param x a data frame with a column named `log`
#' @return A data frame
#' @seealso [logstreams_to_data_frame]
check_for_semicolons_in_strings <- function(x) {
  if (any(nchar(gsub("[^;]", "", x$log)) > 8)) {
    message("    Converting some semicolons to colons...")
    x$log  <- sapply(strsplit(x$log, "'"),
                     function(x) {
                       l <- length(x)
                       if (l < 2) return(x)
                       if (l %% 2 == 0) ii <- seq(2, l, by = 2)
                       else ii <- seq(2, l - 1, by = 2)
                       for (i in ii) x[ii] <- gsub(";", ",", x[ii])
                       return(paste(x, collapse = "'"))
                     })
  }
  return(x)
}
