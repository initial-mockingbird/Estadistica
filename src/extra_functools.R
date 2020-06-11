# Libreria para funcional Dan :D

library(purrr)


# fold Function that uses and accumulator:
fold <- function (x,f,acc) { reduce(c(acc,x),f) }

# Chequea que instancia de cada estructura se debe usar.
isInstance <- function(object,instance){
     fold(class(object), function (x,y) x==instance | y==instance,T)
}

# Useful Notation for functors:
`%<$>%` <- function (x,f) {
     # Checking instances of functor:

     # if it's a list-like structure plain old map works
     if (isInstance(x,"numeric") || isInstance(x,"character") ||
         isInstance(x,"logical") || isInstance(x,"list")  ||
         isInstance(x,"factor")  || isInstance(x,"data.frame")
     ) {
          return(map(x,f))
     }

     # The functor for functions is function composition
     if(isInstance(x,"function")){
          return(compose(x,f))
     }
}

# Useful Notation for applicative functors

# Every Applicative should have a pure that is law abbiding
pure <- function(x,instance) {
     if(instance=="function") {
          return(function (x,y) x)
     }
}

`%<*>%` <- function(fab, fa) {
     # The applicative for functions is a way to combine functions
     # that take two arguments, with functions that take both the same
     # argument
     # notice the type using arrow notation
     # %<*>% :: (r -> a -> b) -> (r -> a) -> (r -> b)
     if(isInstance(fab,"function")){
          return(function(x) fab(x, fa(x)) )
     }

     # cuando necesite el applicativo pa otra estructura lo hago djghh, q lala
}

# map . map Para penetrar cosas de tipo [[a]]
mmap <- function(x,f) {
     return(map(x, function (x) map(x,f)))
}

# map . map . map para cosas tipo [[[a]]]

mmmap <- function(x,f) {
        return (mmap(x,function(x) map(x,f)))
}


# Zip for lists
# zip :: [a] -> [b] -> [(a,b)]
zip <- function(x,y) {
        return(mapply(list,x,y,SIMPLIFY=F) )
}

# uncurry :: (a -> b -> c) -> (a,b) -> c
uncurry <- function(f,t) {
        f(t[1],t[2])

}
