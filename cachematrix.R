## makeCacheMatrix is a "class" that has two properties and four methods:
## Properties:
## - x that stores the matrix itself.
## - inverse stores the inverse of the matrix.
## Methods:
## - set, that sets the matrix stored.
## - get, that returns the matrix stored.
## - setInverse, sets the inverted matrix.
## - getInverse, gets the inverted matrix.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  ## getters and setters
  set <- function(otherMatrix){
    x <<- otherMatrix
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(result) inverse <<- result
  getInverse <- function() inverse
  
  list(set = set, get = get,
       setInverse = setInverse, 
       getInverse = getInverse)
}


## cacheSolve is a function that calculates the inverse, in case
## it is not stored already, or returns it in case it is stored 
## already
cacheSolve <- function(x, ...) {
  ## Get the inverse of the x object using the getInverse method
  inverse <- x$getInverse()
  
  ## If the object has data stored in inverse, don't calculate just return inverse 
  if(!is.null(inverse)){
    message("Getting cached data")
    inverse
  }
  ## If the object doesn't have data stored in inverse, calculate it
  realMatrix <- x$get()
  inverse <- solve(realMatrix, ...)
  x$setInverse(inverse)
  inverse
}
