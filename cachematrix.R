## Created 2/12/2015
## makeCacheMatrix and cacheSolve functions work together to
## speed up the code where inverse of the matrix is computed for the same matrix
## so many times. 

## makeCacheMatrix creates a list that keeps record of inverse of the matrix, if it was computed before
## if the inverse is not computed yet, then it will have a flag (m=NULL), indicating that it is a virgin matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set=set,get = get,
       setinv = setinv,
       getinv = getinv)
}


## the function cacheSolve solve for the inverse of the matrix if the matrix is virgin (ie: inverse not computed before)
## otherwise, it simply returns, previously computed inverse saved by makeChacheMatrixs
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setinv(m)
  m
}
