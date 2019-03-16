## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly 
##(there are also alternatives to matrix inversion that we will not discuss here). 
## Your assignment is to write a pair of functions that cache the inverse of a matrix.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  #create the set function
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  #create the get function
  get <- function() x
  #create the setsovle function
  setsolve <- function(solveResult) m <<- solveResult
  #create the getsolve function
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve, getsolve = getsolve)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  ## if inverse exists return the cached value
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # otherwise compute the invers and cached the result
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
