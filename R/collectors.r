collectors <- function(
    survey,
    page = NULL,
    page_size = NULL,
    start_date = NULL,
    end_date = NULL,
    name = NULL,
    order_asc = NULL,
    fields = NULL,
    api_key = getOption('sm_api_key'),
    oauth_token = getOption('sm_oauth_token')
){
    if(inherits(survey, 'sm_survey'))
        survey <- survey$survey_id
    if(!is.null(api_key)) {
        u <- paste('https://api.surveymonkey.net/v2/surveys/get_collector_list?',
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
    b <- list(survey_id = survey, page = page, page_size = page_size,
              start_date = start_date, end_date = end_date,
              name = name, order_asc = order_asc, fields = as.list(fields))
    nulls <- sapply(b, is.null)
    if(all(nulls))
        b <- '{}'
    else
        b <- toJSON(b[!nulls], auto_unbox = TRUE)
    h <- add_headers(Authorization=token,
                     'Content-Type'='application/json')
    out <- POST(u, config = h, body = b)
    stop_for_status(out)
    content <- content(out, as='parsed')
    if(content$status != 0) {
        warning("An error occurred: ",content$errmsg)
        return(content)
    } else
        lapply(content$data$collectors, `class<-`, 'sm_collector')
}
