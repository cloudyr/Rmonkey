respondentlist <- function(
    survey,
    collector = NULL,
    page = NULL,
    page_size = NULL,
    start_date = NULL,
    end_date = NULL,
    start_modified_date = NULL,
    end_modified_date = NULL,
    name = NULL,
    order_asc = NULL,
    order_by = NULL,
    fields = NULL,
    api_key = getOption('sm_api_key'),
    oauth_token = getOption('sm_oauth_token'),
    ...
){
    if(inherits(survey, 'sm_survey'))
        survey <- survey$survey_id
    if(!is.null(collector) && inherits(collector, 'sm_collector'))
        collector <- collector$collector_id
    if(!is.null(api_key)) {
        u <- paste('https://api.surveymonkey.net/v2/surveys/get_respondent_list?',
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
    if(inherits(start_modified_date, "POSIXct") | inherits(start_modified_date, "Date"))
        start_modified_date <- format(start_modified_date, "%Y-%m-%d %H:%M:%S", tz = "UTC")
    if(inherits(end_modified_date, "POSIXct") | inherits(end_modified_date, "Date"))
        end_modified_date <- format(end_modified_date, "%Y-%m-%d %H:%M:%S", tz = "UTC")
    b <- list(survey_id = survey, collector_id = collector,
              page = page, page_size = page_size,
              start_date = start_date, end_date = end_date,
              start_modified_date = start_modified_date,
              end_modified_date = end_modified_date,
              name = name, order_asc = order_asc, fields = as.list(fields))
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
    if(content$status != 0) {
        warning("An error occurred: ",content$errmsg)
        return(content)
    } else {
        out <- lapply(content$data$respondents, `class<-`, 'sm_respondent')
        return(out)
    }
}

print.sm_respondent <- function(x,...){
    if(!is.null(x$respondent_id))
        cat('Respondent ID:',x$respondent_id,'\n')
    if(!is.null(x$collection_mode)){
        if(!is.null(x$collector_id))
            cat(x$collection_mode,'Collector ID:',x$collector_id,'\n')
    } else{
        if(!is.null(x$collector_id))
            cat('Collector ID:',x$collector_id,'\n')
    }
    if(!is.null(x$email))
        cat('Email:    ',x$email,'\n')
    if(!is.null(x$first_name))
        cat('First:    ',x$first_name,'\n')
    if(!is.null(x$last_name))
        cat('Last:     ',x$last_name,'\n')
    if(!is.null(x$custom_id))
        cat('Custom ID:',x$custom_id,'\n')
    if(!is.null(x$ip_address))
        cat('IP Address:',x$ip_address,'\n')
    if(!is.null(x$status))
        cat(x$status,'\n')
    if(!is.null(x$date_start))
        cat('Started: ',x$date_start,'\n')
    if(!is.null(x$date_modified))
        cat('Modified:',x$date_modified,'\n')
    if(!is.null(x$analysis_url))
        cat('Analysis URL:',x$analysis_url,'\n')
    invisible(x)
}
