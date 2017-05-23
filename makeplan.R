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

propose <- function() {
    e$proposal <- random_meal()
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
