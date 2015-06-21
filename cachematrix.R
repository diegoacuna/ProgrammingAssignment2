## The next two functions calculate the inverse of a matrix and cache the result to optimize
## the futures calculations.


## Creates the cache version of the inverse x matrix passed as a parameter. It returns
## a list of functions to retrieve and set the matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inversepar) inverse <<- inversepar
  getinverse <- function() inverse
  list(set = set, get =get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Return the cached version if it exist or calculate the cached version of the
## inverse of matrix x.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if (!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)

  x$setinverse(inverse)

  return(inverse)
}

#### EXAMPLE RUN ###
# set.seed(1000)
# mat <- matrix(rnorm(10000), nrow=100, ncol=100)
# cachedmat <- makeCacheMatrix(mat)
# cacheSolve(cachedmat)
####################
