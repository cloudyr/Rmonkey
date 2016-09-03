# A Survey Monkey R Client #

[![Build Status](https://travis-ci.org/cloudyr/Rmonkey.png?branch=master)](https://travis-ci.org/cloudyr/Rmonkey)
[![CRAN Version](http://www.r-pkg.org/badges/version/Rmonkey)](http://cran.r-project.org/package=Rmonkey)
![Downloads](http://cranlogs.r-pkg.org/badges/Rmonkey)
[![Project Status: Inactive - The project has reached a stable, usable state but is no longer being actively developed; support/maintenance will be provided as time allows.](http://www.repostatus.org/badges/latest/inactive.svg)](http://www.repostatus.org/#inactive)

**Rmonkey** provides access to [Survey Monkey](https://www.surveymonkey.com/), for the complete integration of survey data collection and analysis into a single, easily reproducible workflow.

## Installation ##

**Rmonkey** is [available on GitHub](http://github.com/cloudyr/Rmonkey) and can (soon) be installed from within R from your favorite CRAN mirror:

```R
install.packages("Rmonkey")
library("Rmonkey")
```

And the latest development version, available here, can be installed directly using  [devtools](http://cran.r-project.org/web/packages/devtools/index.html):

```R
if(!require("devtools")) {
    install.packages("devtools")
    library("devtools")
}
install_github("cloudyr/Rmonkey")
library("Rmonkey")
```

## Setup ##

To use Rmonkey, the user must have a Survey Monkey account, a Mashery Survey Monkey Developer account, and a registered API application. To create a Survey Monkey account, visit https://www.surveymonkey.com/user/sign-in/. To create a Mashery developer account, visit https://developer.surveymonkey.com/member/register. Once registered, it is relatively easy to obtain an API key and secret client ID. It is then also possible to register an API application. This requires a name, OAuth redirect URL, and a brief description. In order to use Rmonkey, the redirect url must be registered as `http://localhost:1410`.

Once everything is registered, the relevant variables can be loaded into R using `options`:

```R
options(sm_api_key = 'YourAPIKey')
options(sm_secret = 'YourAPISecret')
options(sm_client_id = 'YourMasheryDeveloperUsername')
```

Rmonkey uses these values inside `smlogin` to initiate an OAuth2.0 login. Calling `smlogin()`, you will redirected to your web browser, where you will login with your regular Survey Monkey account information. `sm_login` will then store a durable OAuth token in `options('sm_oauth_token')`, which is automatically retrieved in subsequent Rmonkey operations.

This token is currently long-lived (meaning it is valid indefinitely). This means that saving the OAuth token between R sessions will prevent you from having to login each time you load **Rmonkey** and allow you to use the package in non-interactive R sessions. If you have trouble logging in, it is also possible to generate an OAuth token using the [API Console](https://developer.surveymonkey.com/api_console), which can then be manually stored in `options('sm_oauth_token')`. 

## Code Examples ##

Below are some code examples showing how to use the package.

### Creating a survey and retrieving results ###

**Rmonkey** can be used to create surveys (but not distribute them) as well as retrieve the finished results. 

Here's a basic workflow to create a survey from either a survey template or an existing survey:

```R
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

```R
createcollector(new1, name = 'My new survey link')
```

Retrieving results using **Rmonkey** is also relatively easy but involves two steps. First, you need to use `respondentlist` to retrieve the list of respondents to a specific survey (which can be further narrowed to a specific collector on that survey) and then use `getresponses` to further retrieve the actual responses data.

```R
s <- surveylist() # to retrieve the survey id for your survey, if you don't already know it
r <- respondentlist(s[[1]])
g <- getresponses(r)

# convert `g` to data.frame:
as.data.frame(g)
```

`getresponses` returns an object of class "sm_response_list", which has an `as.data.frame` method. Thus to convert the resulting list to a usable dataframe, simply use the standard: `as.data.frame`. Note: Survey Monkey returns variable numbers and full question wordings for each variable. The variable numbers are used to create the data.frame column names and the full question wordings are stored in a "question" attribute for each column; and all variables are encoded as factors. You may want to manually rename and recode the columns before proceeding.

From v0.2.15, Rmonkey can also return a data.frame containing all responses for a survey using just one function:

```R
getallresponses(s[[1]])
```

This way there is no need to iterate through respondents, or manually call the `as.data.frame()` method. This is not widely tested, yet.


### Polling for new responses ###

If you need to pull down recent responses or simply check the status of a survey, **Rmonkey** can do this for you easily. For example, simply to monitor the number of completed surveys, you can simply us `responsecounts` to retrieve the number of completed interviews for a specified survey collector (note that response counts can only be retrieved for a specific collector, not a survey as a whole).


```R
s <- surveylist()
colls <- collectors(s[[1]])
responsecounts(colls[[1]])
```

This can be used to check if additional responses have been received for the specified collector since the last time you checked. To additionally retrieve new responses, you can use a combination of `respondentlist` and `getresponses` to build and update a dataset dynamically. All that's required is recording the last time you check for completed surveys.

```R
dat <- list()
last <- Sys.time() # store last retrieval time
dat[[1]] <- respondentlist(s[[2]], start_modified_date = NULL, fields = c("date_modified","status"))

# wait
Sys.sleep(10)

# get new responses since last poll
dat[[2]] <- respondentlist(s[[2]], start_modified_date = last, fields = c("date_modified","status"))
last <- Sys.time() # update last retrievval time
```

This process can be repeated, possibly in a loop. Then, eventually, the results can be combined into data.frame (or you could build a data.frame at each polling iteration).

```R
plyr::rbind.fill(lapply(dat, function(z) as.data.frame(getresponses(z, survey = s[[1]]))))
```

Note the complexity of the above operation is somewhat unavoidable because SurveyMonkey does not return raw answer data for `getresponses`. Instead it returns a complex mapping of those responses that has to be matched against the survey details (see `surveydetails`). Thus converting the response data into a data.frame actually requires additional API calls. Using the `as.data.frame` method on the list of responses reduces the number of API calls needed for that to happen (i.e., it only retrieves the survey structure one time, as opposed to retrieving it for every single response, thereby avoiding API throttling).


### Literate Report Generation ###

One possible use case for Survey Monkey (or any online survey tool) is the repeated polling of some target population. Perhaps it is website visitors or customers and new data are generated every day, week, or month. If reports need to be generated from these data - and especially if those reports follow a standard template - **Rmonkey** can be particularly helpful for extracting data from Survey Monkey allowing those reports to be generated almost automatically using **knitr**.

A possible workflow is:

 1. Create survey from template
 2. Distribute the survey link manually
 3. Pull responses in literate Rmd document and publish report

```R
# get available templates
tmp <- templates()

survey <- 
createsurvey(tmp[[1]], title = "knitr Demonstration", 
             collector_name = Sys.Date(), type = "email",
             recipients = list(c("thosjleeper@gmail.com")),
             email_replyto = "me@example.com", 
             email_subject = "Take my survey", 
             email_body = "Body of email here. Click my link: [SurveyLink].
                           If you want to opt out, click here: [RemoveLink]")
# go to SurveyMonkey.com and distribute the survey
```

You can then use `createcollector` to create new collectors for each new day, thus separating results for each survey that can easily be retrieved using `getresponses` for that collector.

Then, you can (at some future point in time) generate a simple Rmarkdown document that retrieves and summarizes results auto-magically:

```
---
title: "Survey Results"
output: pdf_document
---

    ```{r, results = "hide", echo = "false"}
    options(sm_api_key = 'YourAPIKey')
    options(sm_secret = 'YourAPISecret')
    options(sm_client_id = 'YourMasheryDeveloperUsername')
    options(sm_oauth_token = 'YourOAuthToken')
    ```

This document contains the survey results:

    ```R
    # retrieve responses
    s <- surveylist()
    survey <- s[[1]]
    # dat <- getallresponses(s[[1]])
    summary(dat)
    # etc.
    ```

Isn't that amazing?

```
