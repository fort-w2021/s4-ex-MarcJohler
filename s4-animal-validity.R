### validity checks
# check general animals
check_animal <- function(object) {
  # assert collection for all violations of requirements
  violations <- checkmate::makeAssertCollection()
  # check length of name and female
  checkmate::assert_character(object@name,
    len = 1,
    add = violations,
    any.missing = FALSE,
    min.chars = 1
  )
  checkmate::assert_flag(object@female, add = violations)
  # check if weight is a positive single number
  checkmate::assert_number(object@weight,
    lower = 0,
    upper = Inf,
    add = violations
  )
  # returns TRUE if there are no errors collected
  violations$getMessages()
}

# check preys
check_prey <- function(object) {
  # assert collection for all violations of prey specific requirements
  violations <- checkmate::makeAssertCollection()
  # depending on the animal check for the specific weight interval
  # note that this has to be done here, since it does not work in check_animal
  # because every animal will have class "animal" when check is done on a
  # specific species
  weight_interval <- switch(class(object),
    mouse = c(0.5, 1),
    rabbit = c(1, 5),
    deer = c(15, 30),
    c(0, Inf)
  )
  checkmate::assert_number(object@weight,
    lower = weight_interval[[1]],
    upper = weight_interval[[2]],
    add = violations
  )
  # depending on the animal check for the specific hide interval
  hide_interval <- switch(class(object),
    mouse = c(0.6, 1),
    rabbit = c(0.3, 0.8),
    deer = c(0.2, 0.7),
    c(0, 1)
  )
  checkmate::assert_number(object@hide,
    lower = hide_interval[[1]],
    upper = hide_interval[[2]],
    add = violations
  )
  # returns TRUE if there are no errors collected
  violations$getMessages()
}

# check predators
check_predator <- function(object) {
  # assert collection for all violations of predator specific requirements
  violations <- checkmate::makeAssertCollection()
  # depending on the animal check for the specific weight interval
  # note that this has to be done here, since it does not work in check_animal
  # because every animal will have class "animal" when check is done on a
  # specific species
  weight_interval <- switch(class(object),
    hawk = c(3, 8),
    lynx = c(20, 60),
    c(0, Inf)
  )
  checkmate::assert_number(object@weight,
    lower = weight_interval[[1]],
    upper = weight_interval[[2]],
    add = violations
  )
  # depending on the animal check for the specific seek interval
  seek_interval <- switch(class(object),
    hawk = c(0.6, 1),
    lynx = c(0.5, 0.9),
    c(0, 1)
  )
  # same goes for seek
  checkmate::assert_number(object@seek,
    lower = seek_interval[[1]],
    upper = seek_interval[[2]],
    add = violations
  )
  # returns TRUE if there are no errors collected
  violations$getMessages()
}
