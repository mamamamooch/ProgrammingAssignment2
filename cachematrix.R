## This R script makes use of makeCacheMatrix and cacheSolve functions
## which are used for caching potentially time-consuming computations--
## such as long vectors requiring the mean of the matrix to solve inverse.

## makeCacheMatrix consists of set, get,  setInverse, and getInverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve is used to get the cache data

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("retrieving cached data")
    return(inv) ## Returns a matrix that is the inverse of 'x'
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}