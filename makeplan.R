library(lubridate)

random_meal <- function(){
    random_plan(1)
}

random_plan <- function(number_of_meals) {
    masterlist = readLines('masterlist')
    recentlist = readLines('recentlist')
    sample(setdiff(masterlist, recentlist),
           number_of_meals, replace = FALSE)
}

is_valid_meal <- function(meal) {
    masterlist = readLines('masterlist')
    recentlist = readLines('recentlist')
    meal %in% setdiff(masterlist, recentlist)
}

list_meals <- function() {
    readLines('masterlist')
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

e = list2env(list(plan = c()))

#'
#' 1. propose a meal
#' 2. propose another or decide to use the proposal
#' 3. continue until you're finished
#'

propose <- function(proposal) {
    if(missing(proposal)) {
        e$proposal <- random_meal()
    } else if(is_valid_meal(proposal)) {
        e$proposal <- proposal
    } else {
        cat('invalid proposal. perhaps you meant:\n')
        print(list_meals()[order(adist(proposal, list_meals()))[1:5]])
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
