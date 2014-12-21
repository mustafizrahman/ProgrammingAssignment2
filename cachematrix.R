# The following two functions calculate the inverse of a matrix x. 
# If the inverse of the matrix has already been calculated it is returned from the cache saving computation time. 
# Otherwise, inverse of the matrixit is calculated and saved in the cache via setImatrix function.
# To see how the two functions work, makeCacheMatrix(x) is called first, then cacheSolve(x).

## --------------------------------------------------------------------------------
# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
## --------------------------------------------------------------------------------

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## --------------------------------------------------------------------------------
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
# If the inverse has already been calculated (and the matrix has not changed), then the inverse is retrieved the cache.
## --------------------------------------------------------------------------------

cacheSolve <- function(x) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data.")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setinverse(inverse)
  inverse
}
