\name{smlogin}
\alias{smlogin}
\title{OAuth Login}
\description{Login into Survey Monkey to generate an OAuth 2.0 token}
\usage{
smlogin(client_id = getOption('sm_client_id'),
        api_key = getOption('sm_api_key'),
        secret = getOption('sm_secret'),
        redirect_uri = 'http://localhost:1410',
        response_type='code')
}
\arguments{
\item{client_id}{Your Mashable developer account username. By default, retrieved from \code{getOption('sm_client_id')}.}
\item{api_key}{Your API key. By default, retrieved from \code{getOption('sm_api_key')}.}
\item{secret}{Your API secret key. By default, retrieved from \code{getOption('sm_secret')}.}
\item{redirect_uri}{Default value is \samp{http://localhost:1410}. No other value is allowed. This must be the redirect URL registered for your application in your Survey Monkey developer account.}
\item{response_type}{Default value is \dQuote{code}. No other values are allowed.}
%  \item{...}{Other arguments. Currently ignored.}
}
\details{Initiate an interactive OAuth 2.0 authentication process by logging into Survey Monkey via a web browser.}
\value{An OAuth 2.0 token object as returned by \code{oauth2.0_token}.}
%\references{}
\author{Thomas J. Leeper}
%\note{}
%\seealso{}
\examples{
\dontrun{smlogin()}
}
%\keyword{}
