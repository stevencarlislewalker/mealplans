library(lubridate)

random_meal <- function(meal_list){
    random_plan(1, meal_list)
}

random_plan <- function(number_of_meals, meal_list) {
  if(missing(meal_list)) {
    masterlist = list_meals(TRUE, FALSE)
  } else {
    masterlist = meal_list
  }
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

search_meals = function(pattern) {
  grep(pattern, list_meals(), ignore.case = TRUE, value = TRUE)
}

e = list2env(list(plan = c()))

#'
#' 0. set the working directory
#' 1. propose a meal
#' 2. propose another or decide to use the proposal
#' 3. continue until you're finished
#'

propose <- function(proposal, pattern) {
  if(missing(pattern)) {
    l = list_meals(TRUE)
  } else {
    l = drop_keywords(search_meals(pattern))
  }
  if(missing(proposal)) {
      e$proposal <- random_meal(l)
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

# himama = readLines('himama_lunch.txt')
# writeLines(unique(sort(unlist(lapply(strsplit(himama, '[0-9]+\ ?servings?'), '[', -1)))), 'himama_lunches.txt')
