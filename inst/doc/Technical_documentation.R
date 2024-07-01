## ----eval=FALSE, include=TRUE-------------------------------------------------
#  # constructor takes any number of arguments arg1, arg2, and so on
#  # and it must use the elipsis ... as final argument
#  new_simdesign <- function(arg1, arg2, ...) {
#  
#      # define generator function in one argument
#      generator = function(n) {
#          # implement data generating mechanism
#          # make use of any argument passed to the new_simdesign constructor
#          # make sure it returns a two-dimensional array
#      }
#  
#      # setup simdesign subclass
#      # make sure to pass generator function and ...
#      # all other information passed is optional
#      dsgn = simdesign(
#          generator = generator,
#          arg1 = arg1,
#          arg2 = arg2,
#          ...
#      )
#  
#      # extend the class attribute
#      class(dsgn) = c("binomial_simdesign", class(dsgn))
#  
#      # return the object
#      dsgn
#  }

## ----echo=FALSE---------------------------------------------------------------
sessionInfo()

