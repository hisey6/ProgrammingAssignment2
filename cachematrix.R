## R Programming: Assignment 2 - These two functions will cache the
## inverse of a matrix

## Function 'makeCacheMatrix' - This function creates a special matrix
## object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function()x
  setInverse <- function(solve)m <<- solve
  getInverse <- function()m
  list(set = set, get = get, setInverse = setInverse, 
       getInverse = getInverse)
}


## Function 'cacheSolve' - This function computes the inverse of the
## matrix return by 'makeCacheMatrix'. If the inverse has already
## been calculated, and not changed, then it retrieves the inverse
## from the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    s <- x$getInverse()
    if(!is.null(s)) {
      message("Getting cached data")
      return(m)
    }
    data <- x$get()
    s <- solve(data,...)
    x$setInverse(s)
    s
}
