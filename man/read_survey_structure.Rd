% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_survey_structure.R
\name{read_survey_structure}
\alias{read_survey_structure}
\title{Reading in text files storing survey structure}
\usage{
read_survey_structure(x)
}
\arguments{
\item{x}{A character vector with file names \strong{or} a data frame with
already read survey structure file.}
}
\value{
A data frame with columns:
\describe{
\item{target.id}{Possible id of a survey interface (HTML) element.}
\item{surveyId}{Survey id (in LimeSurvey database).}
\item{questionId}{Question id (in LimeSurvey database).}
\item{questionFormat}{A letter code of a question format - see
\href{https://manual.limesurvey.org/Question_object_types#Current_question_types}{LimeSurvey documentation page}.}
\item{SGQA}{Question/subquestion/answer \emph{SGQA identifier} - see
\href{https://manual.limesurvey.org/SGQA_identifier}{LimeSurvey documentation page}.}
\item{questionCode}{Question code.}
\item{subquestionCode}{Subquestion code.}
\item{answerCode}{Answer code.}
}
}
\description{
Function is called internally by
\link{separate_logdata_types} and is not exported (i.e. it is not
intended to be called by package users themselves). It reads in text
(tab-delimited) files storing survey structure previously exported from the
\emph{LimeSurvey} web-survey platform (if provided with file names) or passes
down a provided data frame.
}
\seealso{
\link{separate_logdata_types}, \link{separate_returns},
\link{preprocess_input_positions}, \link{preprocess_actions}
}
