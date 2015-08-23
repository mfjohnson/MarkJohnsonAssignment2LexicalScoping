## cachematrix.R
# Author: Mark Johnson
##
## cachematrix provides a pair of methods to support the
## calculation of an inverse matrix and to cache the
## result in the environmental variable 'm'.

## makeCacheMatrix
## Parameter: matrix
## Stores the matrix passed in the
## the function matrix parameter into the functions
## environment variable using lexical scoping.
## Defines 4 subfunctions associated with the 
## cache to support setting and getting the 
## base matrix and the inverse matrix.
##
makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  
  # Stores the base matrix and initializes the
  # calculated inverted matrix to null.
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # Retrieve the initial base matrix
  get <- function() x
  
  ## Stores the calculated inverted matrix
  setmatrix <- function(value) m <<- value
  
  ## If available returns the calculated inverted matrix.  Null if the 
  ## inverted matrix is not yet calculated for this instance
  getmatrix <- function()    m

  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## cacheSolve
## Parameter: makeCacheMatrix instance
## takes the matrix object held by the makeCacheMatrix
## and if there is not a calculated matrix
## value held in cache executes a solve operation,
## otherwise the cached value is passed to the caller
## of this function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}

