## Put comments here that give an overall description of what your
## functions do

## make makeCacheMatrix function to create a specific matrix and store the 
##inverse of the matrix in

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
  }
  get <- function() x
  setinverse <- function(rvrs) inv <<- rvrs
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## CacheSolve solves the inverse of the matrix and in case is already cached, 
##returns the cached value

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
 

