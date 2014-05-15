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
    if(length(respondents)>100){
        respondents <- head(respondents, 100)
        warning("Maximum number of respondents exceeded. Only first 100 used.")
    }
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
        return(structure(content$data, class = 'sm_response_list'))
}

print.sm_response <- function(x, ...){
    if(!is.null(x$respondent_id))
        cat('Responent ID:',x$respondent_id,'\n')
    invisible(x)
}

as.data.frame.sm_response <- function(x, details = NULL, detail_opts = NULL, ...){
    if(is.null(details)){
        details <- do.call('surveydetails', c(survey = x$survey_id, detail_opts))
    } else if(is.character(details)){
        details <- do.call('surveydetails', c(survey = details[1], detail_opts))
    } else if(!inherits(details, 'sm_surveydetails')){
        stop("'details' is not character or an 'sm_surveydetails' object")
    }
    qcount <- x$question_count
    # parse details:
        # extract all questions from the `question` element in all pages
            # `heading` is the display text
            # `type` contains info about each question type
        # extract all answers from the `answers` elements of each subelement of `question`
            # `answer_id` is what is recorded in `sm_response`
            # `text` is the display seen by respondents
            # `answers` is empty for "open_ended" type questions
    # parse into dataframe
        # build dataframe, where variable names are `question_id` and values are `answer_id`
        # recode responses by looking up `question_id` in details and recoding answers
        # rename columns to something
    invisible(x)
}

as.data.frame.sm_response_list <- function(x, details = NULL, detail_opts = NULL, ...){
    if(is.null(details)){
        details <- do.call('surveydetails', c(survey = x[[1]]$survey_id, detail_opts))
    } else if(is.character(details)){
        details <- do.call('surveydetails', c(survey = details[1], detail_opts))
    } else if(!inherits(details, 'sm_surveydetails')){
        stop("'details' is not character or an 'sm_surveydetails' object")
    }
    out <- do.call(rbind, lapply(x, as.data.frame, details = details, ...))
    return(out)
}
