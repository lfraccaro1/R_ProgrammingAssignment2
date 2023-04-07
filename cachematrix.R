## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix function creates a special matrix object that can cache its inverse. It defines four functions: set, get, setinv, and getinv. The set function sets the value of the matrix to the input parameter y and resets the value of the inverse to NULL. The get function retrieves the value of the matrix. The setinv function sets the value of the inverse to the input parameter inverse. The getinv function retrieves the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The cacheSolve function computes the inverse of the matrix that is passed as an argument using the solve function in R. It first checks to see if the inverse has already been calculated and cached. If it has, it retrieves the value of the inverse from the cache and returns it. If the inverse has not been calculated, it calculates the inverse of the matrix, caches the calculated inverse, and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinv(inv)
  inv
}
