getresponses <- function(
    respondents,
    survey,
    api_key = getOption('sm_api_key'),
    oauth_token = getOption('sm_oauth_token'),
    ...
){
    if (missing(survey)) {
        svals <- unique(unlist(lapply(respondents, `[`, "survey_id")))
        if (length(svals) > 1) {
            stop("'respondents' must all come from one survey")
        }
        survey <- svals[1]
    } else {
        if (inherits(survey, 'sm_survey')) {
            survey <- survey$survey_id
        } else {
            stop("'survey' is missing and could not be extracted from 'respondents'")
        }
    }
    if (inherits(respondents, "sm_respondent")) {
        respondents <- respondents$respondent_id
    } else if (is.list(respondents)) {
        respondents <- unname(sapply(respondents, `[`, "respondent_id"))
    }
    if (!is.null(api_key)) {
        u <- paste('https://api.surveymonkey.net/v2/surveys/get_responses?',
                    'api_key=', api_key, sep='')
    } else
        stop("Must specify 'api_key'")
    if (!is.null(oauth_token)) {
        token <- paste('bearer', oauth_token)
    } else {
        stop("Must specify 'oauth_token'")
    }
    if (length(respondents)>100) {
        respondents <- head(respondents, 100)
        warning("Maximum number of respondents exceeded. Only first 100 used.")
    }
    h <- add_headers(Authorization=token,
                     'Content-Type'='application/json')
    b <- toJSON(list(respondent_ids = as.list(respondents), survey_id = survey), auto_unbox = TRUE)
    out <- POST(u, config = h, ..., body = b)
    stop_for_status(out)
    content <- content(out, as='parsed')
    if (content$status != 0) {
        warning("An error occurred: ",content$errmsg)
        return(content)
    } else {
        if (!is.null(content$data)) {
            lapply(content$data, `class<-`, 'sm_response')
            content$data <- lapply(content$data, `attr<-`, 'survey_id', survey)
        }
        return(structure(content$data, class = 'sm_response_list'))
    }
}

print.sm_response <- function(x, ...){
    if (!is.null(x$respondent_id)) {
        cat('Respondent ID:',x$respondent_id,'\n')
    }
    invisible(x)
}

getallresponses <- function(
    survey,
    api_key = getOption('sm_api_key'),
    oauth_token = getOption('sm_oauth_token'),
    wait = 0,
    ...
) {
    r <- respondentlist(survey, api_key = api_key, oauth_token = oauth_token, ...)
    Sys.sleep(wait)
    respondents <- unname(sapply(r, `[`, "respondent_id"))
    Sys.sleep(wait)
    n <- ceiling(length(respondents)/100)
    w <- split(1:length(respondents), rep(1:n, each = 100)[1:length(respondents)])
    out <- list()
    for (i in seq_len(n)) {
        out <- c(out, getresponses(unlist(respondents[w[[i]]]), survey = survey, 
                                   api_key = api_key, oauth_token = oauth_token, ...))
        Sys.sleep(wait)
    }
    class(out) <- 'sm_response_list'
    d <- surveydetails(survey, api_key = api_key, oauth_token = oauth_token, ...)
    as.data.frame(out, details = d)
}
