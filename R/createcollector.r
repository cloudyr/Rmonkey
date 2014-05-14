createcollector <- function(
    survey,
    name,
    type = 'weblink',
    recipients = NULL,
    email_replyto = NULL,
    email_subject = NULL,
    email_body = NULL,
    api_key = getOption('sm_api_key'),
    oauth_token = getOption('sm_oauth_token')
){
    if(type=='weblink'){
        # create_collector endpoint
        if(!is.null(api_key)) {
            u <- paste('https://api.surveymonkey.net/v2/collectors/create_collector?',
                        'api_key=', api_key, sep='')
        } else
            stop("Must specify 'api_key'")
        if(!is.null(oauth_token))
            token <- paste('bearer', oauth_token)
        else
            stop("Must specify 'oauth_token'")
        b <- list(survey_id = survey, collector = list(type = type, name = name))
        b <- toJSON(b)
        h <- add_headers(Authorization=token,
                         'Content-Type'='application/json')
        out <- POST(u, config = h, body = b)
        stop_for_status(out)
        content <- content(out, as='parsed')
        if(content$status==3){
            warning("An error occurred: ",content$errmsg)
            return(content)
        } else {
            structure(content$data$collector, class='sm_collector')
        }
    } else if(type=='email'){
        # send_flow endpoint; doesn't actually send the email???
        if(!is.null(api_key)) {
            u <- paste('https://api.surveymonkey.net/v2/batch/send_flow?',
                        'api_key=', api_key, sep='')
        } else
            stop("Must specify 'api_key'")
        if(!is.null(oauth_token))
            token <- paste('bearer', oauth_token)
        else
            stop("Must specify 'oauth_token'")
        # handle recipients list
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
        b <- list(survey_id = survey,
                  collector = list(type = type, name = name, recipients = recipients),
                  email_message = if(is.null(email_body)) {
                  list(reply_email = email_replyto, subject = email_subject)
                  } else { list(reply_email = email_replyto,
                                            subject = email_subject,
                                            body = email_body)})
        b <- toJSON(b)
        h <- add_headers(Authorization=token,
                         'Content-Type'='application/json')
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
}

print.sm_collector <- function(x, ...){
    if(!is.null(x$name))
        cat('Collector Name:', x$name, '\n')
    if(!is.null(x$collector_id))
        cat('ID:', x$collector_id, '\n')
    if(!is.null(x$type))
        cat('Type:', x$type, '\n')
    if('type' %in% names(x) && x$type=='url')
        cat('URL:', x$url, '\n')
    if(!is.null(x$date_created))
        cat('Date Created: ', x$date_created, '\n')
    if(!is.null(x$date_modified))
        cat('Date Modified:', x$date_modified, '\n')
    if(!is.null(x$open))
        cat('Open?', x$open, '\n\n')
    invisible(x)
}
