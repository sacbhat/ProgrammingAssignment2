
# makeCacheMatrix is a function that returns a list of functions. 
# Its purpose is to store a martix and a cached value of the inverse of the matrix. Contains the following functions:
# Args:
#   x: A matrix (Optional)
#
# Returns:
#   A matrix with functions to get/set value & get/set inverse
# Example :
# mtrx <- matrix(c(0, 1, 2, 3), nrow = 2, ncol = 2, byrow = TRUE)
# mtrx_inv <- makeCacheMatrix(mtrx)
# cacheSolve(mtrx_inv)
#      [,1] [,2]
#[1,] -1.5  0.5
#[2,]  1.0  0.0
# cacheSolve(mtrx_inv)
#inverse is cached
#     [,1] [,2]
#[1,] -1.5  0.5
#[2,]  1.0  0.0

makeCacheMatrix <- function(x = matrix()) {
  # cached inverse of matrix
  inv <- NULL
  
  # get/set for matrix
  get <- function() x
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # get/set for matrix inverse
  getinv <- function() inv
  setinv <- function(inverse) inv <<- inverse
  
  # return list of functions for matrix
  list(get=get, set=set, getinv=getinv, setinv=setinv)
}

# Computes the inverse of a matrix. 
# If inverse is already computed then return that inverse 

cacheSolve <- function(x, ...) {
  inv <- x$getinv()

  # return cached matrix inverse if it is already exists
  if (!is.null(inv)) {
    message("inverse is cached")
    return(inv)
  }
  
  # compute inverse of matrix 
  m <- x$get()
  inv <- solve(m, ...)
  
  # cache inverse
  x$setinv(inv)
  
  # return inverse of matrix
  return(inv)
}