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
    sample(vowels, size = floor(length / 2), replace = TRUE)
  paste(name, collapse = "")
}

# ...for a general animal
animal <- function(name = make_name(), weight, female = TRUE) {
  # generate a new animal and check for attribute limitations
  new("animal", name = name, weight = weight, female = female)
}

##
# ...for a general prey
prey <- function(name = make_name(),
                 weight,
                 female = TRUE,
                 hide = runif(1, 0, 1)) {
  # generate a new prey and check for attribute limitations
  new("prey", name = name, weight = weight, female = female, hide = hide)
}

##
# ...for a mouse
mouse <- function(name = make_name(),
                  weight = runif(1, 0.5, 1),
                  female = TRUE,
                  hide = runif(1, 0.6, 1)) {
  # generate a new prey and check for attribute limitations
  new("mouse", name = name, weight = weight, female = female, hide = hide)
}

# ...for a rabbit
rabbit <- function(name = make_name(),
                   weight = runif(1, 1, 5),
                   female = TRUE,
                   hide = runif(1, 0.3, 0.8)) {
  # generate a new prey and check for attribute limitations
  new("rabbit", name = name, weight = weight, female = female, hide = hide)
}

# ... for a deer
deer <- function(name = make_name(),
                 weight = runif(1, 15, 30),
                 female = TRUE,
                 hide = runif(1, 0.2, 0.7)) {
  # generate a new prey and check for attribute limitations
  new("deer", name = name, weight = weight, female = female, hide = hide)
}

##
# ... for a general predator
predator <- function(name = make_name(),
                     weight,
                     female = TRUE,
                     seek = runif(1, 0, 1)) {
  # generate a new prey and check for attribute limitations
  new("predator", name = name, weight = weight, female = female, seek = seek)
}

##
# ... for a hawk
hawk <- function(name = make_name(),
                 weight = runif(1, 3, 8),
                 female = TRUE,
                 seek = runif(1, 0.6, 1)) {
  # generate a new prey and check for attribute limitations
  new("hawk", name = name, weight = weight, female = female, seek = seek)
}

# ... for a lynx
lynx <- function(name = make_name(),
                 weight = runif(1, 20, 60),
                 female = TRUE,
                 seek = runif(1, 0.5, 0.9)) {
  # generate a new prey and check for attribute limitations
  new("lynx", name = name, weight = weight, female = female, seek = seek)
}
