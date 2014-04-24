## This file contains functions calculating a matrix inverse and storing
## it in a memory cache for later use.

## This function defines a series of primitive function objects
## which set and return cached matrix inverse. The function objects
## are stored in a list which is returned to the caller. This list
## can be used to invoke individual functions in a different context.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, 
       getInverse = getInverse)
  
}


## This function either calculates the given matrix inverse or
## if such exists, pulls it from cache (defined by makeCacheMatrix())
## and returns it to the caller.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
