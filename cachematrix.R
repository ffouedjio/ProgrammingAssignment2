###############################################################################
#                                                                             #
#                 --Caching the Inverse of a Matrix--                         #
#                                                                             #
#   The following pair of functions allows to cache the inverse of a matrix   #
#                                                                             #
###############################################################################

## This first function creates a special "matrix" object which is a list containing 
## a function to set and get the value of the matrix and its inverse.

makeCacheMatrix <- function(X = numeric()) {
  I <- NULL
  set <- function(Y) {
    X <<- Y
    I <<- NULL
  }
  get <- function() X
  setinv <- function(inv) I <<- inv
  getinv <- function() I
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## This second function computes the inverse of the special "matrix" returned by makeCacheMatrix function. 
## If the inverse has already been computed (and the matrix has not changed), then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(X,...) {
  I <- X$getinv()
  if(!is.null(I)) {
    message("getting cached inverse")
    return(I)
  }
  mat <- X$get()
  I <- solve(mat,...)
  X$setinv(I)
  I
}


