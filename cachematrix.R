makeCacheMatrix <- function(x = matrix()) {
 
  INVS <- NULL     # initialize the special "matrix" object where cache its inverse to NULL
  
  # create a matrix in the working environment
  set <- function(y) {
    x <<- y
    INVS <<- NULL
  }
  
  get <- function() x   # get the value of the matrix
  setinverse <- function(inverse) INVS <<- inverse   # invert the matrix and store in cache
  getinverse <- function() INVS   # get the inverted matrix from cache
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}   # return the created functions to the working environment


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

# This function assumes that the matrix is always invertible.

cachesolve <- function(x, ...) {
  INVS <- x$getinverse()    # attempt to get the inverse of the matrix stored in cache
  
  
  # if cached inverse exists, return inverted matrix from cache 
  # otherwise, create a matrix in working environment
  if(!is.null(INVS)) {
    message("getting cached data.")
    return(INVS)
  }
  matrix <- x$get()    # create matrix since it does not exist
  INVS <- solve(matrix)   # get the inverse of the matrix
  x$setinverse(INVS)      
  INVS
}
