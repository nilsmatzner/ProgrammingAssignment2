########################################
#### Caching and inverting matrices ####
##  Coursera Programming Assignment2  ##
########################################

## This is Nils Matzner's sollution to the Courera R-Programming Assignment2
## This file is forked from repository https://github.com/rdpeng/ProgrammingAssignment2

## In makeCacheMatrix x has to be a square matrix
## makeCacheMatrix can cache a matrix with set() or a inverted matrix with setinv()
## These can be recalled with the get() and getinv() function

makeCacheMatrix <- function(x = matrix()) {
      x.inv <- NULL
      set <- function(y){
            x <<- y
            x.inv <<- NULL
      }
      get <- function() x
      setinv <- function(solve) x.inv <<- solve
      getinv <- function() x.inv
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}


## In cacheSolve x has to be an element created with makeCacheMatrix
## It can recall the cache in case it has been set

cacheSolve <- function(x, ...) {
      x.inv <- x$getinv()
      if(!is.null(x.inv)) {
            message("getting cached data")
            return(x.inv)
      }
      data <- x$get() 
      x.inv <- solve(data)
      x$setinv(x.inv)
      x.inv
}

###################
#### CONTAINER ####
###################
# testing data
#mtx <- matrix(1:4, nrow = 2)  # dummy matrix
#mtx.inv <- solve(mtx)         # invert matrix
#mtx %*% mtx.inv               # test inversion
# stat methods for R: http://www.statmethods.net/advstats/matrix.html
# math behind: https://people.richland.edu/james/lecture/m116/matrices/inverses.html
