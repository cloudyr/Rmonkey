getwebhooks <- function(id,
    api_key = getOption('sm_api_key'),
    oauth_token = getOption('sm_oauth_token'),
    ...
){
    if (!is.null(api_key)) {
        if (missing(id)) {
            u <- paste0('https://api.surveymonkey.net/v3/webhooks',
                        '?api_key=', api_key)
        } else {
            u <- paste0('https://api.surveymonkey.net/v3/webhooks/', id,
                        '?api_key=', api_key)
        }
    } else {
        stop("Must specify 'api_key'")
    }
    if (!is.null(oauth_token)) {
        token <- paste('bearer', oauth_token)
    } else {
        stop("Must specify 'oauth_token'")
    }
    out <- GET(u, config = add_headers(Authorization = token), ...)
    stop_for_status(out)
    content <- content(out, as = 'parsed')
    if (content$status != 0) {
        warning("An error occurred: ",content$errmsg)
    }
    structure(content$data, class = 'sm_webhooks')
}

addwebhook <- function(id,
    name,
    type,
    object_type,
    object_ids,
    url,
    api_key = getOption('sm_api_key'),
    oauth_token = getOption('sm_oauth_token'),
    ...
){
    if (!is.null(api_key)) {
        if (missing(id)) {
            u <- paste0('https://api.surveymonkey.net/v3/webhooks',
                        '?api_key=', api_key)
        } else {
            u <- paste0('https://api.surveymonkey.net/v3/webhooks/', id,
                        '?api_key=', api_key)
        }
    } else {
        stop("Must specify 'api_key'")
    }
    if (!is.null(oauth_token)) {
        token <- paste('bearer', oauth_token)
    } else {
        stop("Must specify 'oauth_token'")
    }
    b <- list()
    b$name <- name
    b$event_type <- type
    b$object_type <- object_type
    b$object_ids <- object_ids
    b$subscription_url <- url
    
    out <- POST(u, config = add_headers(Authorization = token), body = b, ...)
    stop_for_status(out)
    content <- content(out, as = 'parsed')
    if (content$status != 0) {
        warning("An error occurred: ",content$errmsg)
    }
    structure(content$data, class = 'sm_webhooks')
}
