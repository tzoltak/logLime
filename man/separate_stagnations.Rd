% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/separate_stagnations.R
\name{separate_stagnations}
\alias{separate_stagnations}
\title{Separating periods of no move from mousemoves actions}
\usage{
separate_stagnations(actions, threshold)
}
\arguments{
\item{actions}{A data frame containing data regarding \emph{actions} -
typically element \emph{actions} of a list returned by
\code{\link{separate_logdata_types}}.}

\item{threshold}{Duration in milliseconds - \emph{mousemove} events lasting
longer than a given value will be separated.}
}
\value{
A data frame with the same columns as input but (probably) with some
additional rows of type \emph{mousemove}.
}
\description{
Functions enables to separate (relatively) long-lasting
\emph{mousemove} \emph{actions} into two distinct \emph{actions}: one describing a period
in which a cursor not moved and another describing actual move.
}
\details{
JavaScript \emph{mousemove} event reports cursor position but is
triggered by moving a cursor. As a consequence respondent not moving a cursor
will be recorded in log-data returned by \link{separate_logdata_types} as
a (relatively) long-lasting (but probably rather \emph{short-distanced})
\emph{mousemove}. This function enables to separate such \emph{actions} into one
covering a period of stagnation (not moving a cursor) and another covering
actual move. Sadly, this can be done only by dividing \emph{action} on the
\strong{arbitrarily chosen threshold} of duration. It is reasonable to set this
threshold to the value of frequency of collecting \emph{mousemove} events that was
used in the \emph{LimeSurvey} log-data collecting applet (typically 100 ms) or
perhaps to a little bigger value.

\strong{Be aware that separating \emph{stagnations} affects values of computed
afterwards cursor move average absolute accelerations!} With \emph{stagnations}
separated, values of these indices are higher. Other cursor indices remain
unaffected by whether \emph{stagnations} were separated or not.
}
\seealso{
\link{separate_logdata_types}, \link{compute_cursor_indices}
}
