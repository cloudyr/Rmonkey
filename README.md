# A Survey Monkey R Client #

[![Build Status](https://travis-ci.org/leeper/Rmonkey.png?branch=master)](https://travis-ci.org/leeper/Rmonkey)

**Rmonkey** provides access to [Survey Monkey](https://www.surveymonkey.com/), for the complete integration of survey data collection and analysis into a single, easily reproducible workflow.

## Installation ##

**Rmonkey** is [available on GitHub](http://github.com/leeper/Rmonkey) and can (soon) be installed from within R from your favorite CRAN mirror:

```
install.packages("Rmonkey")
```

And the latest development version, available here, can be installed directly using  [devtools](http://cran.r-project.org/web/packages/devtools/index.html):

```
# install.packages("devtools")
library("devtools")
install_github("leeper/Rmonkey")
```

## Setup ##

To use Rmonkey, the user must have a Survey Monkey account, a Mashable Survey Monkey Developer account, and a registered API application. To create a Survey Monkey account, visit https://www.surveymonkey.com/user/sign-in/. To create a Mashable developer account, visit https://developer.surveymonkey.com/member/register. Once registered, it is relatively easy to obtain an API key and secret client ID. It is then also possible to register an API application. This requires a name, OAuth redirect URL, and a brief description. In order to use Rmonkey, the redirect url must be registered as `http://localhost:1410`.

Once everything is registered, the relevant variables can be loaded into R using `options`:

```
options(sm_api_key = 'YourAPIKey')
options(sm_secret = 'YourAPISecret')
options(sm_client_id = 'YourMashableDeveloperUsername')
```

Rmonkey uses these values inside `smlogin` to initiate an OAuth2.0 login. Calling `smlogin()`, you will redirected to your web browser, where you will login with your regular Survey Monkey account information. `sm_login` will then store a durable OAuth token in `options('sm_oauth_token')`, which is automatically retrieved in subsequent Rmonkey operations.

This token is currently long-lived (meaning it is valid indefinitely). This means that saving the OAuth token between R sessions will prevent you from having to login each time you load **Rmonkey** and allow you to use the package in non-interactive R sessions. If you have trouble logging in, it is also possible to generate an OAuth token using the [API Console](https://developer.surveymonkey.com/api_console), which can then be manually stored in `options('sm_oauth_token')`. 

## Code Examples ##

Coming soon!

### Creating a survey and retrieving results ###

**Rmonkey** can be used to create surveys (but not distribute them) as well as retrieve the finished results. 

Here's a basic workflow to create a survey from either a survey template or an existing survey:

```
# create a survey and collector from a template
templates <- templatelist()
new1 <- createsurvey(templates[[1]], title = 'My New Survey', recipients = c('recipient1@example.com'),
                     email_replyto = 'myemail@example.com', email_subject = 'Take my survey!')
# create a survey and collector from another survey
s <- surveylist()
new2 <- createsurvey(s[[1]], title = 'My New Survey', recipients = c('recipient1@example.com'),
                     email_replyto = 'myemail@example.com', email_subject = 'Take my survey!')
```

The `createsurvey` function can (currently) only create "email" type collectors. To create a weblink collector, use `createcollector`. The collector URL is returned as part of the "sm_collector" object and can be distributed as desired. Here's an example:
```
createcollector(new1, name = 'My new survey link')
```

Retrieving results using **Rmonkey** is also relatively easy but involves two steps. First, you need to use `respondentlist` to retrieve the list of respondents to a specific survey (which can be further narrowed to a specific collector on that survey) and then use `getresponses` to further retrieve the actual responses data.

```
s <- surveylist() # to retrieve the survey id for your survey, if you don't already know it
r <- respondentlist(s[[1]])
g <- getresponses(r, s[[1]])

# convert `g` to data.frame:
as.data.frame(g)
```

`getresponses` returns an object of class "sm_response_list", which has an `as.data.frame` method. Thus to convert the resulting list to a usable dataframe, simply use the standard: `as.data.frame`. Note: Survey Monkey returns variable numbers and full question wordings for each variable. The variable numbers are used to create the data.frame column names and the full question wordings are stored in a "question" attribute for each column; and all variables are encoded as factors. You may want to manually rename and recode the columns before proceeding.


### Polling for new responses ###

If you need to pull down recent responses or simply check the status of a survey, **Rmonkey** can do this for you easily. For example, simply to monitor the number of completed surveys, you can simply us `responsecounts` to retrieve the number of completed interviews for a specified survey. To additionally retrieve new interviews that have been completed since the last time you checked, you can use a combination of `respondentlist` and `getresponses` to build and update a dataset dynamically. All that's required is recording the last time you check for completed surveys.

INSERT CODE HERE

Based on: https://developer.surveymonkey.com/mashery/polling


### Literate Report Generation ###

One possible use case for Survey Monkey (or any online survey tool) is the repeated polling of some target population. Perhaps it is website visitors or customers and new data are generated every day, week, or month. If reports need to be generated from these data - and especially if those reports follow a standard template - **Rmonkey** can be particularly helpful for extracting data from Survey Monkey allowing those reports to be generated almost automatically using **knitr**.

A possible workflow is:

 1. Create survey from template
 2. Distribute the survey link manually
 3. Pull responses in literate Rmd document and publish report


