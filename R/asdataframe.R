find_details <- function(x, details) {
    if (is.null(details) && !is.null(attr(x, 'survey_id'))) {
        details <- surveydetails(survey = attr(x, 'survey_id'))
    } else if (!is.null(details)) {
        if (inherits(details, 'sm_survey')) {
            details <- details
        } else if (is.character(details)) {
            details <- surveydetails(survey = details[1])
        } else {
            stop("'details' is not character or an 'sm_survey' object")
        }
    } else {
        stop("'details' is missing and cannot be determined automatically")
    }
    return(details)
}

extract_questions_from_pages <- function(details) {
    do.call('c', lapply(details$pages, function(i) i[['questions']]))
}

flatten_options <- function(answers) {
    id <- lapply(answers, `[[`, "answer_id")
    text <- lapply(answers, `[[`, "text")
    setNames(text, id)
}

expand_questions <- function(questions) {
    
    # setup return object
    out <- list()
    
    # extract types
    types <- get_question_types(questions)
    
    # loop through questions, flattening as needed
    not_done <- TRUE
    i <- 1
    j <- 1
    while (not_done) {
        if (types[i] == "single_choice_vertical") {
            out[[j]] <- flatten_options(questions[[i]][["answers"]])
            names(out)[j] <- questions[[i]][["question_id"]]
        } else if (types[i] == "single_choice_horizontal") {
            out[[j]] <- flatten_options(questions[[i]][["answers"]])
            names(out)[j] <- questions[[i]][["question_id"]]
        } else if (types[i] == "multiple_choice_vertical") {
            out[[j]] <- flatten_options(questions[[i]][["answers"]])
            names(out)[j] <- questions[[i]][["question_id"]]
        } else if (types[i] == "multiple_choice_horizontal") {
            out[[j]] <- flatten_options(questions[[i]][["answers"]])
            names(out)[j] <- questions[[i]][["question_id"]]
        } else if (types[i] == "matrix_single") {
            out[[j]] <- questions[[i]]
        } else if (types[i] == "matrix_rating") {
            types <- sapply(questions[[i]][["answers"]], `[[`, "type")
            rows <- which(types %in% c("row", "other"))
            cols <- which(types == "col")
            opts <- flatten_options(questions[[i]][["answers"]][cols])
            for (k in seq_along(questions[[i]][["answers"]])) {
                out[[j]] <- questions[[i]]
                names(out)[j] <- questions[[i]][["question_id"]]
                j <- j + 1
            }
        } else if (types[i] == "matrix_ranking") {
            out[[j]] <- questions[[i]]
        } else if (types[i] == "matrix_menu") {
            out[[j]] <- questions[[i]]
        } else if (types[i] == "open_ended_single") {
            out[[j]] <- questions[[i]][["answers"]]
            names(out)[j] <- questions[[i]][["question_id"]]
        } else if (types[i] == "open_ended_multiple") {
            out[[j]] <- questions[[i]][["answers"]]
            names(out)[j] <- questions[[i]][["question_id"]]
        } else if (types[i] == "demographic") {
            out[[j]] <- questions[[i]]
        } else if (types[i] == "matrix_rating") {
            out[[j]] <- questions[[i]]
        } else {
            out[[j]] <- questions[[i]]
        }
        if (i == length(questions)) {
            not_done <- FALSE
        } else {
            i <- i + 1
            j <- j + 1
        }
    }
    
    # return updated structure
    return(out)
}

get_question_types <- function(questions) {
    sapply(questions, function(i) {
        if (!is.null(i$type$subtype)) {
            setNames(paste0(fam, "_", i$type$subtype), i$question_id)
        } else {
            setNames(fam, i$question_id)
        }
    })
}

extract_answer_choices <- function(questions) {
    answerchoices <- sapply(questions, function(i) {
                        out <- list()
                        for (k in seq_along(i$answers)) {
                            if (i$type$family == "matrix") {
                                if (i$type$subtype == "rating") {
                                    if (i$answers[[k]]$type == "other") {
                                        out[[k]] <- setNames(i$answers[[k]]$text, i$answers[[k]]$answer_id)
                                    } else {
                                        # exclude "col" values from matrix questions
                                        if (i$answers[[k]]$type == "row") {
                                            out[[k]] <- setNames(i$answers[[k]]$text, i$answers[[k]]$answer_id)
                                        } 
                                    }
                                    out[[k]] <- setNames(i$answers[[k]]$text, i$answers[[k]]$answer_id)
                                } else if (i$type$subtype == "menu") {
                                    if (i$answers[[k]]$type == "col") {
                                        tmp_txt <- unlist(lapply(i$answers[[k]]$items, `[`, "text"))
                                        tmp_ans <- unlist(lapply(i$answers[[k]]$items, `[`, "answer_id"))
                                        out[[k]] <- setNames(tmp_txt, tmp_ans)
                                        rm(tmp_txt)
                                        rm(tmp_ans)
                                    }
                                }
                            } else {
                                out[[k]] <- setNames(i$answers[[k]]$text, i$answers[[k]]$answer_id)
                            }
                        }
                        return(unlist(out))
                     })
    answerchoices <- unlist(do.call(c, answerchoices))
    return(answerchoices)
}

as.data.frame.sm_response <- function(x, row.names, optional, details = NULL, stringsAsFactors = FALSE, ...){
    # find details if not supplied
    details <- find_details(x, details)
    
    # extract all questions from the `question` element in all pages
    questions <- extract_questions_from_pages(details)
    
    # extract all answers from the `answers` elements of each subelement of `question`
        # `answer_id` is what is recorded in `sm_response`
        # `text` is the display seen by respondents
        # `answers` is empty for "open_ended" type questions
    answerchoices <- extract_answer_choices(questions)
    
    ## here, need to expand so multi-column/multi-row questions are duplicated
    #questions <- expand_questions(questions)
    
    # `qtypes` contains info about each question type
    qtypes <- get_question_types(questions)
    
    # set variable names
    varnames <- sapply(questions, function(i) {
        # `heading` is the display text
        setNames(i$heading, i$question_id)
    })
    
    # response data
    respondent <- x[["respondent_id"]]
    answers <- setNames(lapply(x[["questions"]], `[[`, "answers"),
                        sapply(x[["questions"]], `[[`, "question_id"))
    
    
    # extract question_ids
    question_ids <- unlist(sapply(x$questions, `[`, 'question_id'))
    # count number of answers per question
    nanswers <- integer()
    for (i in seq_along(x$questions)) {
        nanswers[i] <- length(x$questions[[i]]$answers)
    }
    rm(i)
    # create vector of answer names by repeating question names `nanswers` times each
    answer_names <- rep(question_ids, nanswers)
    
    # recode responses by looking up `question_id` in details and recoding answers
    responses <- character()
    for (i in seq_along(x$questions)) {
        if (qtypes[question_ids[i]] %in% c('single_choice','multiple_choice')) {
            tmp <- unname(answerchoices[unlist(x$questions[[i]]$answers)])
        } else if (qtypes[question_ids[i]] %in% c('matrix_rating')) {
            # this extracts `col` values as answer options
            tmp <- unname(answerchoices[unlist(lapply(x$questions[[i]]$answers, `[[`, "col"))])
        } else if (qtypes[question_ids[i]] %in% c('matrix_menu')) {
            tmp <- unname(answerchoices[unlist(lapply(x$questions[[i]]$answers, `[[`, "col_choice"))])
        } else if (qtypes[question_ids[i]] %in% c('open_ended')) {
            tmp <- unname(unlist(lapply(x$questions[[i]]$answers, `[`, "text")))
        } else {
            tmp <- unname(unlist(x$questions[[i]]$answers))
        }
        responses <- c(responses, tmp)
    }
    rm(tmp)
    responses <- setNames(responses, answer_names)
    
    unique_names <- unique(names(responses))
    for (i in seq_along(unique_names)) {
        cnt <- names(responses)[names(responses) %in% unique_names[i]]
        if (length(cnt) > 1) {
            names(responses)[names(responses) %in% unique_names[i]] <- 
                paste0(unique_names[i], ".", seq_along(cnt))
        }
    }
    out <- setNames(as.data.frame(matrix(unlist(responses), nrow=1), 
                                  stringsAsFactors = stringsAsFactors), names(responses))
    
    # rename columns to something
    for (i in seq_along(out)) {
        attr(out[,i], 'question') <- unname(varnames[answer_names[i]])
    }
    return(out)
}

as.data.frame.sm_response_list <- function(x, row.names, optional, details = NULL, stringsAsFactors = FALSE, ...){
    if (is.null(details) && !is.null(attr(x[[1]], 'survey_id'))) {
        details <- surveydetails(survey = attr(x[[1]], 'survey_id'))
    } else if (!is.null(details)) {
        if (inherits(details, 'sm_survey')) {
            details <- details
        } else if (is.character(details)) {
            details <- surveydetails(survey = details[1])
        } else {
            stop("'details' is not character or an 'sm_survey' object")
        }
    } else {
        stop("'details' is missing and cannot be determined automatically")
    }
    tmp <- lapply(x, as.data.frame, details = details, stringsAsFactors = stringsAsFactors, ...)
    out <- rbind.fill(tmp)
    return(out)
}

