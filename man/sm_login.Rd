\name{smlogin}
\alias{smlogin}
\title{OAuth Login}
\description{Login into Survey Monkey to generate an OAuth 2.0 token}
\usage{
smlogin()
}
\arguments{
\item{client_id}{Your Mashable developer account username. By default, retrieved from \code{getOption('sm_client_id')}.}
\item{api_key}{Your API key. By default, retrieved from \code{getOption('sm_api_key')}.}
\item{secret}{Your API secret key. By default, retrieved from \code{getOption('sm_secret')}.}
\item{redirect_uri}{Default value is \dQuote{http://localhost:1410}. No other value is allowed. This must be the redirect URL registered for your application in your Survey Monkey developer account.}
\item{response_type}{Default value is \dQuote{code}. No other values are allowed.}
%  \item{...}{Other arguments. Currently ignored.}
}
\details{The particular objects returned will vary in structure depending on type of recall. As a result, attempting to build a rectangular dataset from different types of recall data may prove complicated. It is recommended to retrieve recalls of only one type if the goal is to \code{\link{rbind}} the list of results into a matrix or data.frame.}
\value{A list containing one or more objects of class \dQuote{recall}.}
%\references{}
\author{Thomas J. Leeper}
%\note{}
%\seealso{}
\examples{
\dontrun{smlogin()}
}
%\keyword{}
