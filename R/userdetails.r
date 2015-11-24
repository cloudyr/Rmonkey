userdetails <- function(
    api_key = getOption('sm_api_key'),
    oauth_token = getOption('sm_oauth_token'),
    ...
){
    if(!is.null(api_key)) {
        u <- paste('https://api.surveymonkey.net/v2/user/get_user_details?',
                    'api_key=', api_key, sep='')
    } else
        stop("Must specify 'api_key'")
    if(!is.null(oauth_token))
        token <- paste('bearer', oauth_token)
    else
        stop("Must specify 'oauth_token'")
    out <- POST(u, config = add_headers(Authorization=token), ...)
    stop_for_status(out)
    content <- content(out, as='parsed')
    if(content$status != 0)
        warning("An error occurred: ",content$errmsg)
    structure(content$data$user_details, class='sm_userdetails')
}
