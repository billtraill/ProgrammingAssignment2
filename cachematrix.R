## functions to create a cached matrix solve its inverse.
## 

## creates a cached matrix instance for use with cacheSolve 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(i) inv <<- i
  getinverse<- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## takes a cache matrix (returned by makeCacheMatrix) and returns its inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  i <- solve(data,...)
  x$setinverse(i)
  i
}
