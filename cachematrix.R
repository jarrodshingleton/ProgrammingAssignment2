## Put comments here that give an overall description of what your
## functions do

## This function sets up the cached matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmat <- function(mat) m <<- mat
  getmat <- function() m
  list(set = set, get = get,
       setmat = setmat,
       getmat = getmat)
}

## This function solves the cached matrix.

cacheSolve <- function(x, ...) {
  m <- x$getmat()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmat(m)
  m
}

