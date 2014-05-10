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
    oauth_token = getOption('sm_oauth_token')
){
    if(!is.null(api_key)) {
        u <- paste('https://api.surveymonkey.net/v2/surveys/get_respondent_list?',
                    'api_key=', api_key, sep='')
    } else
        stop("Must specify 'api_key'")
    if(!is.null(oauth_token))
        token <- paste('bearer', oauth_token)
    else
        stop("Must specify 'oauth_token'")
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
        b <- toJSON(b[!nulls])
    h <- add_headers(Authorization=token,
                     'Content-Type'='application/json')
    out <- POST(u, config = h, body = b)
    stop_for_status(out)
    content <- content(out, as='parsed')
    if(content$status==3) {
        warning("An error occurred: ",content$errmsg)
        return(content)
    } else {
        lapply(content$data$respondents, `class<-`, 'sm_respondent')
    }
}

print.sm_respondent <- function(x,...){
    # set fields
    invisible(x)
}