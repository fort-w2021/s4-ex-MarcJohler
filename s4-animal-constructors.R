### validity checks 
# function to check if argument has length 1
check_argument_welldefined <-  function(argument_value, argument_name) {
  argument_length <- length(argument_value)
  if (argument_length != 1) {
    stop(paste(argument_name, " is length ", argument_length, ". Should be 1", 
               sep = ""))
  }
}

# check general animals 
check_animal <- function(object, weight_interval = c(0, Inf)) {
  # check length of name, weight and female
  check_argument_welldefined(object@name, "name")
  check_argument_welldefined(object@weight, "weight")
  check_argument_welldefined(object@female, "female")
  # check if name is a "real" string with more than 0 letters
  if (nchar(object@name) == 0) {
    stop('invalid class "animal" object. Animal needs a "name".')
  }
  # check if weight is a positive number 
  checkmate::assert_number(object@weight, 
                           lower = weight_interval[1], 
                           upper = weight_interval[2])
  object
}

# check preys
check_prey <- function(object, weight_interval = c(0, Inf), 
                       hide_interval = c(0, 1)) {
  # check basic requirements for animals
  check_animal(object, weight_interval)
  # same goes for hide
  checkmate::assert_number(object@hide, 
                           lower = hide_interval[1], 
                           upper = hide_interval[2])
  object
}

# check predators
check_predator <- function(object, weight_interval = c(0, Inf), 
                           seek_interval = c(0, 1)) {
  # check basic requirements for animals
  check_animal(object, weight_interval)
  # same goes for seek
  checkmate::assert_number(object@seek, 
                           lower = seek_interval[1], 
                           upper = seek_interval[2])
  object
}


### constructor functions...
# random pronouncable strings with length <length>
make_name <- function(length = 7) {
  vowels <- c("a", "e", "i", "o", "u")
  consonants <- setdiff(letters, vowels)
  name <- character(length)
  name[1] <- sample(toupper(consonants), 1)
  name[seq(3, length, by = 2)] <-
    sample(consonants, size = ceiling(length / 2) - 1, replace = TRUE)
  name[seq(2, length, by = 2)] <-
    sample(vowels, size = floor(length / 2), replace = TRUE
    )
  paste(name, collapse = "")
}

# ...for a general animal
animal <- function(name = make_name(), weight, female = TRUE) {
  # generate a new animal and check for attribute limitations
  check_animal(new("animal", name, weight, female))
}

##
# ...for a general prey
prey <- function(name = make_name(), 
                 weight, 
                 female = TRUE, 
                 hide = runif(1, 0, 1)) {
  # generate a new prey and check for attribute limitations
  check_prey(new("prey", name = name, weight = weight, 
                 female = female, hide = hide),
             weight_interval = c(0, Inf),
             hide_interval = c(0, 1))
}

##
# ...for a mouse
mouse <- function(name = make_name(), 
                  weight = runif(1, 0.5, 1), 
                  female = TRUE, 
                  hide = runif(1, 0.6, 1)) {
  # generate a new prey and check for attribute limitations
  check_prey(new("mouse", name = name, weight = weight,
                 female = female, hide = hide),
             weight_interval = c(0.5, 1),
             hide_interval = c(0.6, 1))
}

# ...for a rabbit
rabbit <- function(name = make_name(), 
                   weight = runif(1, 1, 5), 
                   female = TRUE, 
                   hide = runif(1, 0.3, 0.8)) {
  # generate a new prey and check for attribute limitations
  check_prey(new("rabbit", name = name, weight = weight,
                 female = female, hide = hide),
             weight_interval = c(1, 5),
             hide_interval = c(0.3, 0.8))
}

# ... for a deer
deer <- function(name = make_name(), 
                 weight = runif(1, 15, 30), 
                 female = TRUE, 
                 hide = runif(1, 0.2, 0.7)) {
  # generate a new prey and check for attribute limitations
  check_prey(new("deer", name = name, weight = weight,
                 female = female, hide = hide),
             weight_interval = c(15, 30),
             hide_interval = c(0.2, 0.7))
}

##
# ... for a general predator
predator <- function(name = make_name(), 
                     weight, 
                     female = TRUE, 
                     seek = runif(1, 0, 1)) {
  # generate a new prey and check for attribute limitations
  check_predator(new("predator", name = name, weight = weight,
                     female = female, seek = seek),
                 weight_interval = c(-Inf, Inf),
                 seek_interval = c(0, 1))
}

##
# ... for a hawk
hawk <- function(name = make_name(), 
                 weight = runif(1, 3, 8), 
                 female = TRUE, 
                 seek = runif(1, 0.6, 1)) {
  # generate a new prey and check for attribute limitations
  check_predator(new("hawk", name = name, weight = weight,
                     female = female, seek = seek),
                 weight_interval = c(3, 8),
                 seek_interval = c(0.6, 1))
}

# ... for a hawk
lynx <- function(name = make_name(), 
                 weight = runif(1, 20, 60), 
                 female = TRUE, 
                 seek = runif(1, 0.5, 0.9)) {
  # generate a new prey and check for attribute limitations
  check_predator(new("lynx", name = name, weight = weight,
                     female = female, seek = seek),
                 weight_interval = c(20, 60),
                 seek_interval = c(0.5, 0.9))
}
