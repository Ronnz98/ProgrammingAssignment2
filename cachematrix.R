## Put comments here that give an overall description of what your
## functions do

## This function creates a "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  myInverse <- NULL
  
  set <- function(y) {
    x <<- y
    myInverse <<- NULL  
  }
  
  get <- function() x
  setinv <- function(invert) myInverse <<- invert
  getinv <- function() myInverse
  
  list(
    set = set,
    get = get,
    setinv = setinv,
    getinv = getinv
    )    
}


## The  function computes the inverse of the "matrix" returned by makeCacheMatrix above. 


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invi <- x$getinv()
  
  if(!is.null(invi)) {
    message("Getting the cached data")
    return(invi)
  }
  
  data <- x$get()
  invi <- solve(data, ...)
  x$setinv(invi)
  invi
}
