# These functions provide a means of caching the inverse of a matrix.
# The first time the inverse is calls, solve is called, and the value
# is stored and returned. On later calls, the stored value is returned,
# provided the matrix did not change.

# makeCacheMatrix makes a list of functions to set/get the matrix and to
# set/get the inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# cacheSolve runs solve if the cached value is NULL or the matrix has changed
# since the last call. Otherwise, the cached value is returned.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
