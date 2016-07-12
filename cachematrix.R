## These functions enable the retrieval of a previously calculated matrix inverse
## This is useful since it avoids recomputation of the inverse if the matrix has not changed

## makeCacheMatrix stores the original matrix and a set of related functions

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatinv <- function(matinv) m <<- matinv
  getmatinv <- function() m
  list(set = set, get = get,
       setmatinv = setmatinv,
       getmatinv = getmatinv)
}


## cacheSolve computes the inverse of the matrix unless it has been already calculated

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getmatinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatinv(m)
  m
}
