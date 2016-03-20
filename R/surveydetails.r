surveydetails <- function(
    survey,
    api_key = getOption('sm_api_key'),
    oauth_token = getOption('sm_oauth_token'),
    ...
){
    if(inherits(survey, 'sm_survey'))
        survey <- survey$survey_id
    if(!is.null(api_key)) {
        u <- paste('https://api.surveymonkey.net/v2/surveys/get_survey_details?',
                    'api_key=', api_key, sep='')
    } else
        stop("Must specify 'api_key'")
    if(!is.null(oauth_token))
        token <- paste('bearer', oauth_token)
    else
        stop("Must specify 'oauth_token'")
    h <- add_headers(Authorization=token,
                     'Content-Type'='application/json')
    b <- toJSON(list(survey_id = survey), auto_unbox = TRUE)
    out <- POST(u, config = h, ..., body = b)
    stop_for_status(out)
    content <- content(out, as='parsed')
    if(content$status != 0) {
        warning("An error occurred: ",content$errmsg)
        return(content)
    } else
        structure(content$data, class='sm_survey')
}

surveyquestions <- function(
    survey,
    details,
    api_key = getOption('sm_api_key'),
    oauth_token = getOption('sm_oauth_token'),
    ...
){
    if (!missing(survey)) {
        d <- surveydetails(survey, api_key = api_key, oauth_token = oauth_token, ...)
    } else {
        d <- details
    }
    questions <- unlist(unlist(lapply(d$pages, `[`, "questions"), recursive = FALSE), recursive = FALSE)
    n <- unname(unlist(lapply(questions, `[`, "question_id")))
    w <- unname(unlist(lapply(questions, `[`, "heading")))
    structure(w, names = n, class = c("character", "sm_surveyquestions"))
}

surveypreview <- function(details) {
    browseURL(details$preview_url)
}
