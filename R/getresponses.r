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

as.data.frame.sm_response <- function(x, details = NULL, detail_opts = NULL,.var_names = NULL, ..){
    if(is.null(details)){
        details <- do.call('surveydetails', c(survey = x$survey_id, detail_opts))
    } else if(is.character(details)){
        details <- do.call('surveydetails', c(survey = details[1], detail_opts))
    } else if(!inherits(details, 'sm_surveydetails')){
        stop("'details' is not character or an 'sm_surveydetails' object")
    }
    qcount <- x$question_count
    # extract all questions from the `question` element in all pages
    questions <- do.call('c', lapply(details$pages, function(i) i[['questions']]))
    # `heading` is the display text
    varnames <- sapply(questions, function(i) setNames(i$heading, i$question_id))
    # `type` contains info about each question type
    qtypes <- sapply(questions, function(i) setNames(i$type$family, i$question_id))
    # extract all answers from the `answers` elements of each subelement of `question`
        # `answer_id` is what is recorded in `sm_response`
        # `text` is the display seen by respondents
        # `answers` is empty for "open_ended" type questions
    answerchoices <- sapply(questions, function(i) {
                    sapply(i$answers, function(k) {
                        setNames(k$text, k$answer_id)
                    })
                })
    answerchoices <- setNames(answerchoices, names(varnames))
    # parse details:
    # parse into dataframe
    question_ids <- unlist(sapply(x$questions, `[`, 'question_id'))
    responses <- sapply(x$questions, function(i) {
        # potentially handle different variable types better
        unname(unlist(i$answers))
    })
    responses <- setNames(responses, question_ids)
    lapply(responses, function(i){
        if(sum(grepl('answer',names(i)))>1)
            
    })
        # build dataframe, where variable names are `question_id` and values are `answer_id`
        # recode responses by looking up `question_id` in details and recoding answers
        # rename columns to something
    out <- as.data.frame(matrix(ncol=length(questions)))
    if(is.null(var_names)) {
        out <- setNames(out, gsub('[[:punct:][:space:]]','',varnames))
    } else if(ncol(out) == length(var_names)){
        out <- setNames(out, var_names)
    } else {
        warning("Due to length mismatch, 'var_names' ignored!")
    }
    return(out)
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
