\name{as.data.frame.sm}
\alias{as.data.frame.sm_response}
\alias{as.data.frame.sm_response_list}
\title{Parse responses to dataframe}
\description{Parse responses to a data.frame using a \code{sm_surveydetails} object.}
\usage{
\method{as.data.frame}{sm_response}{x, details = NULL, details_opts = NULL, ...}
\method{as.data.frame}{sm_response_list}{x, details = NULL, details_opts = NULL, ...}
}
\arguments{
\item{x}{An object of class \code{sm_response} or \code{sm_response_list}.}
\item{details}{An object of class \code{sm_surveydetails}, a survey ID number (possibly returned by \code{\link{surveylist}}), or \code{NULL}. The latter two will trigger a \code{\link{surveydetails}} request.}
\item{...}{Other arguments. Currently ignored.}
}
\details{Parses an \code{sm_response} object, or list of such objects, into a dataframe using a \code{sm_surveydetails} object to structure the result.}
\value{A data.frame.}
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
as.data.frame(a, s[[1]])
}
}
%\keyword{}
