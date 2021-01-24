# define generic function "meet"
setGeneric("meet", function(object1, object2, ...) {
  standardGeneric("meet")
})

# paste methods for all cases
# ignoring
animals_ignore <- function(object1, object2) {
  paste(class(object1)[[1]],
    " '", object1@name, "' & ",
    class(object2)[[1]],
    " '", object2@name,
    "' ignore each other",
    sep = ""
  )
}
# sniffing
animals_sniff <- function(object1, object2) {
  paste(class(object1)[[1]],
    " '", object1@name, "' & ",
    class(object2)[[1]],
    " '", object2@name,
    "' sniff each others' butts",
    sep = ""
  )
}
# mating
animals_mate <- function(object1, object2) {
  paste(class(object1)[[1]],
    " '", object1@name, "' & ",
    class(object2)[[1]],
    " '", object2@name,
    "' make sweet, sweet love",
    sep = ""
  )
}
# fighting
animals_fight <- function(object1, object2) {
  paste(class(object1)[[1]],
    " '", object1@name, "' & ",
    class(object2)[[1]],
    " '", object2@name,
    "' fight for territory",
    sep = ""
  )
}
# killing
animals_kill <- function(object1, object2) {
  paste(class(object1)[[1]],
    " '", object1@name,
    "' kills and eats ",
    class(object2)[[1]],
    " '", object2@name, "'",
    sep = ""
  )
}
# escaping
animals_escape <- function(object1, object2) {
  paste(class(object2)[[1]],
    " '", object2@name,
    "' escapes from ",
    class(object1)[[1]],
    " '", object1@name, "'",
    sep = ""
  )
}
# gazing
animals_gaze <- function(object) {
  paste(class(object)[[1]],
    " '", object@name,
    "' gazes at ",
    ifelse(object@female, "her", "his"),
    " reflection in a puddle",
    sep = ""
  )
}

## define methods...
# ... for two general animals meeting each other (used only for callNextMethod())
setMethod(
  "meet", signature(object1 = "animal", object2 = "animal"),
  function(object1, object2) {
    # 50:50 chance for either sniffing or ignoring each other
    case <- rbinom(1, 1, 0.5)
    if (case == 0) {
      return(animals_ignore(object1, object2))
    }
    animals_sniff(object1, object2)
  }
)

# ... for two preys meeting each other
setMethod(
  "meet", signature(object1 = "prey", object2 = "prey"),
  function(object1, object2) {
    # if both animals are the same, the animal is gazing at itself
    if (identical(object1, object2)) {
      return(animals_gaze(object1))
    }
    # if same species (same class) and different sex
    if (class(object1)[[1]] == class(object2)[[1]]) {
      # 0.5 probability for them to mate
      case <- rbinom(1, 1, 0.5)
      if (case == 1) {
        return(animals_mate(object1, object2))
      }
    }
    # otherwise 50:50 chance to either sniff or ignore
    # (animal - animal - method)
    callNextMethod()
  }
)

# ... for two predators meeting each other
setMethod(
  "meet", signature(object1 = "predator", object2 = "predator"),
  function(object1, object2) {
    # if both animals are the same, the animal is gazing at itself
    if (identical(object1, object2)) {
      return(animals_gaze(object1))
    }
    # if same species (same class) and different sex
    if (class(object1)[[1]] == class(object2)[[1]]) {
      # 0.5 probability for them to mate
      case <- rbinom(1, 1, 0.5)
      if (case == 1) {
        return(animals_mate(object1, object2))
      }
      return(animals_fight(object1, object2))
    }
    # otherwise 1/3 probability to fight
    case <- rbinom(1, 1, 1 / 3)
    if (case == 1) {
      return(animals_fight(object1, object2))
    }
    # otherwise 50:50 chance to either sniff or ignore
    # (animal - animal - method)
    callNextMethod()
  }
)

# ... for a predator meeting a prey
setMethod(
  "meet", signature(object1 = "predator", object2 = "prey"),
  function(object1, object2) {
    # if both animals are the same, the animal is gazing at itself
    if (identical(object1, object2)) {
      return(animals_gaze(object1))
    }

    weight1 <- object1@weight
    weight2 <- object2@weight
    # if the weights of both animals are in a certain relation
    if (weight2 >= 0.05 * weight1 && weight2 <= 0.7 * weight1) {
      # the hunt begins with success probability given by this formula:
      death_prob <- min(1, max(0, 0.6 + object1@seek - object2@hide))
      case <- rbinom(1, 1, death_prob)
      # in case of success the predator kills the prey,
      # otherwise the latter escapes
      if (case == 1) {
        return(animals_kill(object1, object2))
      }
      return(animals_escape(object1, object2))
    }
    # otherwise 50:50 chance to either sniff or ignore
    # (animal - animal - method)
    callNextMethod()
  }
)

# ...and vice versa
setMethod(
  "meet", signature(object1 = "prey", object2 = "predator"),
  function(object1, object2) {
    # just switch positions
    meet(object2, object1)
  }
)
