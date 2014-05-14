createsurvey <- function(
    template = NULL,
    survey = NULL,
    title,
    collector_name = NULL,
    type = 'email', # only 'email' is allowed
    recipients = NULL,
    email_reply = NULL,
    email_subject = NULL,
    email_body = NULL,
    api_key = getOption('sm_api_key'),
    oauth_token = getOption('sm_oauth_token')
){
    if(!is.null(api_key)) {
        u <- paste('https://api.surveymonkey.net/v2/batch/create_flow?',
                    'api_key=', api_key, sep='')
    } else
        stop("Must specify 'api_key'")
    if(!is.null(oauth_token))
        token <- paste('bearer', oauth_token)
    else
        stop("Must specify 'oauth_token'")
    # handle recipients list
    if(length(recipients)>10000){
        respondents <- head(recipients, 10000)
        warning("Maximum number of recipients exceeded. Only first 10000 used.")
    }
    recipients <- lapply(recipients, function(x){
        if(is.null(names(x))) {
            if(length(x)==1)
                return(c(email=x[1]))
            else if(length(x)==2)
                return(c(email=x[1], first_name=x[2]))
            else if(length(x)==3)
                return(c(email=x[1], first_name=x[2], last_name=x[3]))
            else if(length(x)==4)
                return(c(email=x[1], first_name=x[2],
                         last_name=x[3], custom_id=x[4]))
        } else
            return(x)
    })
    if(!is.null(template) & !is.null(survey)) {
        stop("Only 'template' xor 'survey' is allowed.")
    } else if(!is.null(template)){
        b <- list(template_id = template, survey_title = title,
                  collector = list(type = type, name = collector_name,
                                   recipients = recipients),
                  email_message = if(is.null(email_body)) {
                  list(subject = email_subject, reply_email = email_reply)
                  } else { list(reply_email = email_reply,
                                            subject = email_subject,
                                            body = email_body)})
    } else if(!is.null(survey)){
        b <- list(from_survey_id = survey, survey_title = title,
                  collector = list(type = type, name = collector_name,
                                   recipients = recipients),
                  email_message = if(is.null(email_body)) {
                  list(subject = email_subject, reply_email = email_reply)
                  } else { list(reply_email = email_reply,
                                            subject = email_subject,
                                            body = email_body)})
    }
    b <- toJSON(list(survey=b))
    #return(b)
    h <- add_headers(Authorization=token, 'Content-Type'='application/json')
    out <- POST(u, config = h, body = b)
    stop_for_status(out)
    content <- content(out, as='parsed')
    if(content$status==3){
        warning("An error occurred: ",content$errmsg)
        return(content)
    } else {
        content <- content$data
        class(content$collector) <- 'sm_collector'
        class(content$survey) <- 'sm_survey'
        return(content)
    }
}
