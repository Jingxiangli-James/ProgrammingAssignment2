# Caching the Inverse of a Matrix:
# Matrix inversion is usually a costly computation and there may be some 
# benefit to caching the inverse of a matrix rather than compute it repeatedly.
# Below are a pair of functions that are used to create a special object that 
# stores a matrix and caches its inverse.


# The first function, makeCacheMatrix creates a special "matrix" object
# which is really a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the Inverse of the matrix
# 4. get the Inverse of the martix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


# This function calculates the inverse of the special "matrix" created by
# makeCacheMatrix above.
# First it will check to see if the inverse matrix has already been calculated.
# If so, it gets the inverse matrix from the cache and skips the calculation.
# Otherwise, it calculates the inverse matrix of the data
# and set the inverse matrix in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}

