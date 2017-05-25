library(lubridate)

random_meal <- function(){
    random_plan(1)
}

random_plan <- function(number_of_meals) {
    masterlist = list_meals(TRUE, FALSE)
    recentlist = list_meals(TRUE, TRUE)
    sample(setdiff(masterlist, recentlist),
           number_of_meals, replace = FALSE)
}

is_valid_meal <- function(meal) {
    masterlist = drop_keywords(list_meals())
    recentlist = readLines('recentlist')
    meal %in% setdiff(masterlist, recentlist)
}

list_meals <- function(drop_keywords = FALSE, just_recent = FALSE) {
    l = readLines(ifelse(just_recent, 'recentlist', 'masterlist'))
    if(drop_keywords) l = drop_keywords(l)
    l
}

use_meals <- function(meals) {
    write(meals, 'recentlist', append = TRUE)
}

clear_recentlist <- function() {
    if(file.exists('recentlist')) {
        file.remove('recentlist')
    }
    file.create('recentlist')
}

suggested_list <- function(keywords, ...) {
    l = lapply(keywords, agrep, x = list_meals(), value = TRUE, ...)
    r = Reduce(union, l)
    s = colSums(outer(unlist(l), r, '=='))
    r = r[order(s, decreasing = TRUE)]
    sapply(strsplit(r, ';'), '[', 1)
}

drop_keywords <- function(meals) {
    sapply(strsplit(meals, ';'), '[', 1)
}

e = list2env(list(plan = c()))

#'
#' 1. propose a meal
#' 2. propose another or decide to use the proposal
#' 3. continue until you're finished
#'

propose <- function(proposal) {
    l = list_meals(TRUE)
    if(missing(proposal)) {
        e$proposal <- random_meal()
    } else if(is_valid_meal(proposal)) {
        e$proposal <- proposal
    } else {
        cat('invalid proposal. perhaps you meant:\n')
        print(agrep(proposal, l, ignore.case = TRUE, value = TRUE))
        stop()
    }
    cat('current proposal: ', e$proposal, '\n')
}

use <- function() {
    cat('adding ', e$proposal, ' to the plan\n')
    e$plan <- c(e$plan, e$proposal)
}

finish <- function() {
    print('here is your meal:')
    print(e$plan)
    use_meals(e$plan)
}
