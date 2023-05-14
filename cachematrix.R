## makeCacheMatrix creates a matrix from a specific matrix, and to save the 
## inverse of this matrix
## cacheSolve gets the inverse of a specific matrix if already cached 
## otherwise compute the inverse of this specific matrix and caches it

## makeCacheMatrix function creates a matrix from a specific matrix 
## and stores the inverse of the matrix in a cache

makeCacheMatrix <- function(x = matrix()) {
  ## Initializing the inverse matrix to NULL
  inverseMatrix <- NULL
  
  ## Setting the x matrix to a specific matrix y
  set <- function(y) {
    x <<- y
  }
  
  ## Getting the assigned specific matrix
  get <- function() x
  
  ## Setting the inverse matrix
  setinverse <- function(invMat) inverseMatrix <<- invMat
  
  ## Getting the inverse matrix
  getinverse <- function() inverseMatrix
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## CacheSolve:
## - returns the cached inverse matrix of x if it is already computed
## - solves the inverse of the matrix x in case it is not yet computed 
##  and cached,and saves the inverse matrix in the cache

cacheSolve <- function(x, ...) {
  ## get the inverse matrix of x
  inverseMatrix <- x$getinverse()
  
  ## returns the inverse matrix of x in case it is already cached
  if(!is.null(inverseMatrix)) {
    message("getting cached data")
    return(inverseMatrix)
  }
  
  ## Computes the inverse matrix of x in case it is not yet computed and cached
  data <- x$get()
  inverseMatrix <- solve(data, ...)
  
  ## Caches the inverse matrix of x
  x$setinverse(inverseMatrix)
  inverseMatrix
}
 

