templates <- function(
    page = NULL,
    page_size = NULL,
    language_id = NULL,
    category_id = NULL,
    only_mine = TRUE,
    fields = NULL,
    api_key = getOption('sm_api_key'),
    oauth_token = getOption('sm_oauth_token')
){
    if(!is.null(api_key)) {
        u <- paste('https://api.surveymonkey.net/v2/templates/get_template_list?',
                    'api_key=', api_key, sep='')
    } else
        stop("Must specify 'api_key'")
    if(!is.null(oauth_token))
        token <- paste('bearer', oauth_token)
    else
        stop("Must specify 'oauth_token'")
    b <- list(page = page, page_size = page_size,
              language_id = language_id, category_id = category_id,
              show_only_available_to_current_user = only_mine,
              fields = as.list(fields))
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
        if('upgrade_info' %in% names(content$data)){
            lapply(content$data$upgrade_info$restrictions, function(x){
                message(x$message)
            })
        }
        lapply(content$data$templates, `class<-`, 'sm_template')
    }
}

print.sm_template <- function(x,...){
    # set fields
    invisible(x)
}