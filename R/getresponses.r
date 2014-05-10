getresponses <- function(
    respondents,
    survey,
    api_key = getOption('sm_api_key'),
    oauth_token = getOption('sm_oauth_token')
){
    if(!is.null(api_key)) {
        u <- paste('https://api.surveymonkey.net/v2/surveys/get_responses?',
                    'api_key=', api_key, sep='')
    } else
        stop("Must specify 'api_key'")
    if(!is.null(oauth_token))
        token <- paste('bearer', oauth_token)
    else
        stop("Must specify 'oauth_token'")
    if(length(respondents)>100)
        warning("Maximum number of respondents exceeded. Only first 100 used.")
    h <- add_headers(Authorization=token,
                     'Content-Type'='application/json')
    b <- toJSON(list(survey_id = survey, respondent_ids = as.list(respondents)))
    out <- POST(u, config = h, body = b)
    stop_for_status(out)
    content <- content(out, as='parsed')
    if(content$status==3) {
        warning("An error occurred: ",content$errmsg)
        return(content)
    } else
        lapply(content$data, `class<-`, 'sm_response')
}


print.sm_surveydetails <- function(x, ...){
    #
    invisible(x)
}