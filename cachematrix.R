## Functions for storing a matrix and its inverse 


## Creates a matrix-like object that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){ 
    x <<- y
    m <<- NULL
  }##set
  get <- function() x 
  setinverse <- function(solve) inv <<- solve 
  getinverse <- function() inv 
  list(set = set, get = get, setinverse = setinverse, 
       getinverse = getinverse)

}##makeCacheMatrix


## Returns the inverse of a matrix
## Calculates the inverse if not already calculated

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }##if
  temp <- x$get()
  inv <- solve(temp, ...)
  x$setinverse(inv)
  inv
}##cacheSolve
