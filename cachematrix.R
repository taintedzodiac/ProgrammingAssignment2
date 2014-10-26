## The two function defined herein are capable of creating, caching,
## and retreiving the inverse


## makeCacheMatrix creates a special "matrix" object 
## that can cache its inverse.
##
## Four sub-functions are defined:
## set - sets the matrix
## get - gets the matrix that was previously set
## setInverse - caches the inverse of the matrix
## getInverse - retrieves the inverse from cache
##
## A and B are matrices
## I is the calculated inverse

makeCacheMatrix <- function(A = matrix()) {
  I <- NULL
  set <- function(B) {
    A <<- B
    I <<- NULL
  }
  get <- function() A
  setInverse <- function(inverse) I <<- inverse
  getInverse <- function() I
  list(set = set, get = get,
    setInverse = setInverse,
    getInverse = getInverse)
}


## cacheSolve calculates the inverse of a matrix (A)
##
## It first checks to see if the inverse was
## previously calculated, and if so retrieves
## the cached results. Otherwise it calculates
## an inverse as normal.

cacheSolve <- function(A, ...) {
  I <- A$getInverse()
  if(!is.null(I)) {
    message("getting cached inverse")
    return(I)
  }
  data <- A$get()
  I <- solve(data, ...)
  A$setInverse(I)
  I
}
