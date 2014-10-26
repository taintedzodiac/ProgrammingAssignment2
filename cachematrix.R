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
  ## Initialize the inverse cache
  I <- NULL
  
  ## Set the matrix
  set <- function(B) {
    A <<- B
    I <<- NULL
  }
  
  ## Get the matrix that's been previously set
  get <- function() A
  
  ## Cache the inverse
  setInverse <- function(inverse) I <<- inverse
  
  ## Retrieve cached inverse
  getInverse <- function() I
  
  ## Expose the sub-functions
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
  ## Check to see if the cached inverse exists
  ## If it does, return it
  I <- A$getInverse()
  if(!is.null(I)) {
    message("getting cached inverse")
    return(I)
  }
  
  ## Get the matrix
  data <- A$get()
  
  ## Use solve() to find the inverse, then cache it
  I <- solve(data, ...)
  A$setInverse(I)
  
  ## Return the calculated inverse
  I
}
