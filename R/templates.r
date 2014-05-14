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
    if(!is.null(x$template_id))
        cat('Template ID:',x$template_id,'\n')
    if(!is.null(x$title))
        cat('Title:',x$title,'\n')
    if(!is.null(x$short_description))
        cat('Description:',x$short_description,'\n')
    #if(!is.null(x$long_description))
    #    cat(x$long_description,'\n')
    if(!is.null(x$language_id))
        cat(x$language_id,'\n')
    if(!is.null(x$is_available_to_current_user))
        cat('Available?',x$is_available_to_current_user,'\n')
    if(!is.null(x$is_featured))
        cat('Featured? ',x$is_featured,'\n')
    if(!is.null(x$is_certified))
        cat('Certified?',x$is_certified,'\n')
    if(!is.null(x$page_count))
        cat('Pages:',x$page_count,'\n')
    if(!is.null(x$question_count))
        cat('Questions:',x$question_count,'\n')
    if(!is.null(x$preview_url))
        cat('Preview URL:',x$preview_url,'\n')
    if(!is.null(x$category_name)){
        if(!is.null(x$category_id))
            cat('Category ',x$category_name,' (',x$category_id,'):\n',sep='')
        if(!is.null(x$category_description))
            cat(x$category_description,'\n')
    }
    if(!is.null(x$date_created))
        cat('Date created: ',x$date_created,'\n')
    if(!is.null(x$date_modified))
        cat('Date modified:',x$date_modified,'\n')
    invisible(x)
}
