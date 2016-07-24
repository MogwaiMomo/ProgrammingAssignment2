## The following code allows the user to input a matrix return the
## inverse of that matrix. The first function, makeCacheMatrix, 
## returns a list of functions (via a closure) that allow the second 
## function, cacheSolve, to cache the inverse matrix it calculates, as
## well as check for it and use it in the future when invoked with the
## same input matrix.

## Example input variable to use (if desired)
mtx <- matrix(c(3,5,4,6,7,8,9,12,5), nrow=3, ncol=3)


## makeCacheMatrix returns a list of functions that can be used 
## cache & retrieve values for inverse of the matrix x. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse_mtx) inv <<- inverse_mtx
  getinv <- function() inv
  # closure obj to be accessed within cacheSolve()
  return(list(set = set, 
              get = get, 
              setinv=setinv, 
              getinv=getinv))
}

## cacheSolve is a function that calculates the inverse of a matrix 
## passed into the function makeCacheMatrix(). If a value for the
## inverse exists already (via a cache), it retrieves the cached value 
## and skips the calculation.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
