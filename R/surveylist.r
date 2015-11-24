surveylist <- function(
    page = NULL,
    page_size = NULL,
    start_date = NULL,
    end_date = NULL,
    title = NULL,
    recipient_email = NULL,
    order_asc = NULL,
    fields = NULL,
    api_key = getOption('sm_api_key'),
    oauth_token = getOption('sm_oauth_token'),
    ...
){
    if(!is.null(api_key)) {
        u <- paste('https://api.surveymonkey.net/v2/surveys/get_survey_list?',
                    'api_key=', api_key, sep='')
    } else
        stop("Must specify 'api_key'")
    if(!is.null(oauth_token))
        token <- paste('bearer', oauth_token)
    else
        stop("Must specify 'oauth_token'")
    if(inherits(start_date, "POSIXct") | inherits(start_date, "Date"))
        start_date <- format(start_date, "%Y-%m-%d %H:%M:%S", tz = "UTC")
    if(inherits(end_date, "POSIXct") | inherits(end_date, "Date"))
        end_date <- format(end_date, "%Y-%m-%d %H:%M:%S", tz = "UTC")
    b <- list(page = page, page_size = page_size,
              start_date = start_date, end_date = end_date,
              title = title, recipient_email = recipient_email,
              order_asc = order_asc, fields = as.list(fields))
    nulls <- sapply(b, is.null)
    if(all(nulls))
        b <- '{}'
    else
        b <- toJSON(b[!nulls], auto_unbox = TRUE)
    h <- add_headers(Authorization=token,
                     'Content-Type'='application/json')
    out <- POST(u, config = h, ..., body = b)
    stop_for_status(out)
    content <- content(out, as='parsed')
    if(content$status != 0){
        warning("An error occurred: ",content$errmsg)
        return(content)
    } else 
        lapply(content$data$surveys, `class<-`, 'sm_survey')
}

print.sm_survey <- function(x, ...){
    if(!is.null(x$title)) {
        if(is.list(x$title))
            cat('Survey Title:', x$title$text, '\n')
        else
            cat('Survey Title:', x$title, '\n')
    }
    if(!is.null(x$survey_id))
        cat('ID:', x$survey_id, '\n')
    if(!is.null(x$language_id))
        cat('Language:', x$language_id, '\n')
    if(!is.null(x$question_count))
        cat('No. of Questions:', x$question_count, '\n')
    if(!is.null(x$num_responses))
        cat('Respondents:', x$num_responses, '\n')
    if(!is.null(x$preview_url))
        cat('Preview URL:', x$preview_url, '\n')
    if(!is.null(x$analysis_url))
        cat('Analysis URL:', x$analysis_url, '\n')
    if(!is.null(x$date_created))
        cat('Date Created: ', x$date_created, '\n')
    if(!is.null(x$date_modified))
        cat('Date Modified:', x$date_modified, '\n')
    if(!is.null(x$pages))
        cat('Survey Pages:', length(x$pages), '\n')
    invisible(x)    
}
