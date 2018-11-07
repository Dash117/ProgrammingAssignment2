## These functions calculate caching of a matrix and its inverse.
## cache <- makeCacheMatrix(matrix) creates an initialized cache.
## cache$get() returns the stored matrix.
## cache$set(matrix) changes the stored matrix.
## cacheSolve(cache) solves the inverse.


## Initialize the matrix.

makeCacheMatrix <- function(x = matrix()) {
  invrs <- NULL
  set <- function(y) {
    x <<- y
    invrs <<- NULL
  }
  get <- function() x
  setinvs <- function(inv) invrs <<- inv
  getinvs <- function() invrs
  list(set = set, get = get,
       setinvs = setinvs, getinvs = getinvs)
}


## Returns the inverse of the cached matrix.

cacheSolve <- function(x, ...) {
  invrs <- x$getinvs()
  if (!is.null(xi)) {
    message("getting cached inverse")
    return(invrs)
  }
  xm <- x$get()
  invrs <- solve(xm, ...)
  x$setinvs(invrs)
  return(invrs)
}