\name{Rmonkey-package}
\alias{Rmonkey-package}
\docType{package}
\title{A Survey Monkey R Client}
\description{Rmonkey provides access to \href{https://www.surveymonkey.com/}{Survey Monkey}, for the complete integration of survey data collection and analysis into a single, easily reproducible workflow.}
\details{
To use Rmonkey, the user must have a Survey Monkey account, a Mashable Survey Monkey Developer account, and a registered API application. To create a Survey Monkey account, visit \url{https://www.surveymonkey.com/user/sign-in/}. To create a Mashable developer account, visit \url{https://developer.surveymonkey.com/member/register}. Once registered, it is relatively easy to obtain an API key and secret client ID. It is then also possible to register an API application. This requires a name, OAuth redirect URL, and a brief description. In order to use Rmonkey, the redirect url must be registered as \samp{http://localhost:1410}.

Once everything is registered, the relevant variables can be loaded into R using \code{options}:

\code{options(sm_api_key = 'YourAPIKey')}
\code{options(sm_secret = 'YourAPISecret')}
\code{options(sm_client_id = 'YourMashableDeveloperUsername')}

Rmonkey uses these values inside \code{\link{smlogin}} to initiate an OAuth2.0 login. Calling \code{smlogin}, you will redirected to your web browser, where you will login with your regular Survey Monkey account information. \code{smlogin} will then store a durable OAuth token in \code{options('sm_oauth_token')}, which is automatically retrieved in subsequent Rmonkey operations. If you have trouble logging in, it is also possible to generate an OAuth token using \href{https://developer.surveymonkey.com/api_console}{the API Console}, which can then be manually stored in \code{options('sm_oauth_token')}.
}
\author{
Thomas J. Leeper

Maintainer: Thomas J. Leeper <thosjleeper@gmail.com>
}
\references{
\href{https://developer.surveymonkey.com/Home}{API Documentation}

\href{https://developer.surveymonkey.com/mashery/data_types}{Documentation of variable values used in responses}
}
\keyword{ package }
%\seealso{}
%\examples{}
