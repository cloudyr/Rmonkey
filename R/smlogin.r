smlogin <- function(client_id = getOption('sm_client_id'),
                    api_key = getOption('sm_api_key'),
                    secret = getOption('sm_secret'),
                    redirect_uri = 'http://localhost:1410',
                    response_type='code'){
    if(is.null(client_id))
        stop("Must supply developer username as 'client_id'")
    if(is.null(api_key))
        stop("Must supply developer API key as 'api_key'")
    if(is.null(secret))
        stop("Must supply developer secret key as 'secret'")
    a <- list(response_type = response_type,
              redirect_uri = redirect_uri,
              client_id = client_id,
              api_key = api_key)
    a <- paste(names(a), curl_escape(a), sep='=', collapse='&')
    e <- structure(list(authorize = 'https://api.surveymonkey.net/oauth/authorize',
                        access = 'https://api.surveymonkey.net/oauth/token'), class='oauth_endpoint')
    e$authorize <- paste(e$authorize,a,sep='?')
    e$access <- paste(e$access,'?api_key=',api_key,sep='')
    smapp <- oauth_app('surveymonkey', client_id, secret)
    
    token <- oauth2.0_token(e, smapp, use_oob = FALSE, cache = FALSE)
    if('error' %in% names(token$credentials)){
        warning('OAuth error ', token$credentials$error,
                ': ', token$credentials$error_description, sep='')
    } else
        options('sm_oauth_token' = token$credentials$access_token)
    invisible(token)
}
