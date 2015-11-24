getresponses <- function(
    respondents,
    survey,
    api_key = getOption('sm_api_key'),
    oauth_token = getOption('sm_oauth_token'),
    ...
){
    if(inherits(respondents, "sm_respondent")) {
        respondents <- respondents$respondent_id
    } else if(is.list(respondents)) {
        respondents <- unname(sapply(respondents, `[`, "respondent_id"))
    }
    if(inherits(survey, 'sm_survey'))
        survey <- survey$survey_id
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
    b <- toJSON(list(respondent_ids = as.list(respondents), survey_id = survey), auto_unbox = TRUE)
    out <- POST(u, config = h, ..., body = b)
    stop_for_status(out)
    content <- content(out, as='parsed')
    if(content$status != 0) {
        warning("An error occurred: ",content$errmsg)
        return(content)
    } else {
        if(!is.null(content$data)) {
            lapply(content$data, `class<-`, 'sm_response')
            content$data <- lapply(content$data, `attr<-`, 'survey_id', survey)
        }
        return(structure(content$data, class = 'sm_response_list'))
    }
}

print.sm_response <- function(x, ...){
    if(!is.null(x$respondent_id))
        cat('Respondent ID:',x$respondent_id,'\n')
    invisible(x)
}

as.data.frame.sm_response <- function(x, row.names, optional, details = NULL, ...){
    if(is.null(details) && !is.null(attr(x, 'survey_id'))) {
        details <- surveydetails(survey = attr(x, 'survey_id'))
    } else if(!is.null(details)){
        if(inherits(details, 'sm_survey')){
            details <- details
        } else if(is.character(details)){
            details <- surveydetails(survey = details[1])
        } else {
            stop("'details' is not character or an 'sm_survey' object")
        }
    } else {
        stop("'details' is missing and cannot be determined automatically")
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
    answerchoices <- unlist(do.call(c, answerchoices))
    # recode responses by looking up `question_id` in details and recoding answers
    responses <- mapply(function(i, type) {
        # potentially handle different variable types better
        if(type %in% c('single_choice','multiple_choice'))
            unname(answerchoices[match(unlist(i$answers), names(answerchoices))])
        else
            unname(unlist(i$answers))
    }, x$questions, qtypes)
    question_ids <- unlist(sapply(x$questions, `[`, 'question_id'))
    responses <- setNames(responses, question_ids)
    # convert multiple choice to multiple variables
    len <- sapply(responses, length)
    outvarnames <- unlist(mapply(function(cnt, name) {
                            if(cnt>1)
                                paste(name,1:cnt,sep='.')
                            else
                                name
                        }, len, names(len)))
    # build dataframe, where variable names are `question_id` and values are `answer_id`
    out <- setNames(as.data.frame(matrix(unlist(responses), nrow=1)), outvarnames)
    
    # rename columns to something
    for(i in seq_along(out)) {
        attr(out[,i], 'question') <-
            varnames[pmatch(strsplit(names(out)[i],'\\.')[[1]][1], names(varnames))]
    }
    return(out)
}

as.data.frame.sm_response_list <- function(x, row.names, optional, details = NULL, ...){
    if(is.null(details) && !is.null(attr(x[[1]], 'survey_id'))) {
        details <- surveydetails(survey = attr(x[[1]], 'survey_id'))
    } else if(!is.null(details)){
        if(inherits(details, 'sm_survey')){
            details <- details
        } else if(is.character(details)){
            details <- surveydetails(survey = details[1])
        } else {
            stop("'details' is not character or an 'sm_survey' object")
        }
    } else {
        stop("'details' is missing and cannot be determined automatically")
    }
    tmp <- lapply(x, as.data.frame, details = details, ...)
    out <- rbind.fill(tmp)
    return(out)
}
