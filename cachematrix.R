##Creates a special "matrix", which is really a list containing a function to
##
##set/get the value of the matrix
##set/get the value of the inverse matrix

makeCacheMatrix <- function(X) {
  inv <- NULL
  set <- function(Y) {
    X <<- Y
    inv <<- NULL
  }
  get <- function() X
  setinv <- function(inver) inv <<- inver
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Сalculates the inverse of the special "matrix" 
## If the inverse has already been calculate, it gets the inverse
## from the cache and skips the computation.

cacheSolve <- function(X) {
  inv <- X$getinv()
  if(!is.null(inv)) {
    return(inv)
  }
  data <- X$get()
  inv <- solve(data)
  X$setinv(inv)
  inv
}