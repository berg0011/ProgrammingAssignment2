## cachematrix is a set of functions to take a matrix and wrap it with
## a set of functions to set and get the inverse of the matrix, allowing
## the inverse to be cached in memory

## Creates a cachmatrix from a matrix, allowing the inverse of the
## matrix to by cached.  Get the inverse by calling function cacheSolve
## param - matrix to wrap with inverse functions
## returns - a cachematrix
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Returns the inverse of the cachematrix using the solve function,
## if the inverse was previously solved, returns the inverse from
## the cache
## params - cachematrix to solve, any additional arguments to pass
##          to the solve function
## returns - the inverse of the cachematrix
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
