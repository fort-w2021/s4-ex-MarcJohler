### class for general animals 
setClass("animal", 
         representation(name = "character", 
                        weight = "numeric", 
                        female = "logical"))

## animal sub-class for preys (Beutetiere)
setClass("prey",
         representation(hide = "numeric"),
         contains = "animal")

# prey-subclass for mice
setClass("mouse",
         contains = "prey")

# prey-subclass for deers
setClass("deer",
         contains = "prey")

# prey-subclass for rabbits
setClass("rabbit",
         contains = "prey")

## animal sub-class for predators
setClass("predator",
         representation(seek = "numeric"), 
         contains = "animal")

# predator-subclass for hawks
setClass("hawk",
         contains = "predator")

# predator-subclass for lynx'
setClass("lynx",
         contains = "predator")

