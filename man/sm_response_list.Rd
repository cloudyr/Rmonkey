\name{as.data.frame.sm_response}
\alias{as.data.frame.sm_response}
\alias{as.data.frame.sm_response_list}
\title{Parse responses to dataframe}
\description{Parse responses to a data.frame using a \code{sm_surveydetails} object.}
\usage{
\method{as.data.frame}{sm_response}(x, details = NULL, \dots)
\method{as.data.frame}{sm_response_list}(x, details = NULL, \dots)
}
\arguments{
\item{x}{An object of class \code{sm_response} or \code{sm_response_list}.}
\item{details}{If \code{NULL}, the method looks for a \dQuote{survey_id} attribute in \code{x}. Otherwise, \code{details} must be supplied as an object of class \code{sm_surveydetails} or a survey ID number (possibly returned by \code{\link{surveylist}}). The first and third of these possibilities will trigger (and therefore require) a \code{\link{surveydetails}} request.}
\item{\dots}{Other arguments. Currently ignored.}
}
\details{Parses an \code{sm_response} object, or list of such objects, into a dataframe using a \code{sm_surveydetails} object to structure the result.}
\value{A data.frame. Variable names are based upon the Survey Monkey internal \code{question_id} numbers. Question wordings are stored in a \dQuote{question} attribute in each variable. The user will have to manually rename variables.}
%\references{}
\author{Thomas J. Leeper}
%\note{}
%\seealso{}
\examples{
\dontrun{
smlogin()
s <- surveylist()
r <- respondentlist(s[[1]])

# get all responses (up to 100)
a <- getresponses(lapply(r[[1]], function(x) `[`, 'respondent_id'), s[[1]])
as.data.frame(a)
}
}
%\keyword{}
