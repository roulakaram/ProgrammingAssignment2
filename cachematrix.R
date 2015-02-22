## Put comments here that give an overall description of what your
## functions do
# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  ##
  ## function makeCacheMatrix returns a matrix that is the inverse of 'x'
  ##
  inv <- NULL
  # Set the value of the matrix:
  # store the matrix in the cache variable 'x';
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # Get the value of the matrix:
  # read the matrix from the cache variable 'x';
  get <- function() x
  # Set the inverse of the matrix:
  # store the inverse matrix in the cache variable 'inv';
  setinv <- function(m_inverse) inv <<- m_inverse
  # Get the inverse matrix from the cache variable 'inv';
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}
## Write a short comment describing this function
#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
# should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ##
  ## Return a matrix that is the inverse of 'x'
  ##
  ## get the inverse matrix stored, 'inv'
  inv <- x$getinv()
  ## if 'inv' is not null, return 'inv'
  if(!is.null(inv)) {
    message("getting cached inverse matrix")
    return(inv)
  }
  ## otherwise get the matrix 'x' and assign it to 'matrix_to_invert'
  matrix_to_invert <- x$get()
  ## use the function solve() to compute the inverse of 'matrix_to_invert'
  ## and assign it to 'inv'
  inv <- solve(matrix_to_invert, ...)
  ## store the inverse matrix 'inv' in the space of 'x'
  x$setinv(inv)
  ## return 'inv'
  inv
}