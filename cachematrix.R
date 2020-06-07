## The makeCacheMatrix and cacheSolve function will help you to do operations in a faster way by helping you to cache time-consuming operations.
##The makeCacheMatrix function is able to create a matrix object that is able to cache its inverse.

makeCacheMatrix<- function(x = matrix()){
  inv<- NULL
  set<- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {inv <<- inverse }
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse =setInverse, getInverse = getInverse)
}

## The cacheSolve function computes the inverse of the matrix created while using makeCacheMatrix. 
## However, if the inverse has been calculated or there is no change in the matrix, cacheSolve function retrieves the inverse from the cache. 
cacheSolve<- function(x, ...) {
  inv<- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv<- solve(mat, ...)
  x$setInverse(inv)
  inv
}
