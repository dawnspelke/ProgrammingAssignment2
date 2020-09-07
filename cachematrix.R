## These functions, in tandem, cache the inverse of a matrix.


## This function creates a matrix object that can cache its inverse matrix.
## The object contains four functions: set, get, setinverse, and getinverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## This function calculates the inverse of the matrix object from makeCacheMatrix.
## If the inverse is already cached, this function will return the inverse from
## memory, rather than recalculate.

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
