### class for general animals
setClass("animal",
  representation(
    name = "character",
    weight = "numeric",
    female = "logical"
  ),
  validity = check_animal
)

## animal sub-class for preys (Beutetiere)
setClass("prey",
  representation(hide = "numeric"),
  contains = "animal",
  validity = check_prey
)

# prey-subclass for mice
setClass("mouse",
  contains = "prey",
  validity = check_prey
)

# prey-subclass for deers
setClass("deer",
  contains = "prey",
  validity = check_prey
)

# prey-subclass for rabbits
setClass("rabbit",
  contains = "prey",
  validity = check_prey
)

## animal sub-class for predators
setClass("predator",
  representation(seek = "numeric"),
  contains = "animal",
  validity = check_predator
)

# predator-subclass for hawks
setClass("hawk",
  contains = "predator",
  validity = check_predator
)

# predator-subclass for lynx'
setClass("lynx",
  contains = "predator",
  validity = check_predator
)
