# Thie file provides two functions that allow for caching the inverse of a matrix
# makeCacheMatrix wraps a matrix, along with functions to access the matrix's inverse
# cacheSolve takes a matrix, and returns the inverse (cached value, if possible; else computes it)


# Takes a matrix as argument, and provides functions to access the inverse
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() {
    x
  }
  setInverse <- function(y) {
    inverse <<- y
  }
  getInverse <- function() {
    inverse
  }
  
  list (set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# cacheSolve takes a list returned from makeCacheMatrix, and
# returns the cached value for matrix inverse if available, otherwise it computes the inverse
cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if (!is.null(inverse)) {
    message("returning cached value for the inverse")
    return(inverse)
  }
  
  m <- x$get()
  inverse <- solve(m)
  x$setInverse(inverse)
  inverse
}
