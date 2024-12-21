## A pair of functions (makeCacheMatrix and cacheSolve) to cache the inverse of a matrix.
## The purpose of the functions is to create a special matrix to calculate and store
## the inverse of the matrix to simplify calculations.
## Assumption: that the matrix supplied is always invertible (square matrix with an inverse).

## Creates a special "matrix" object that can cache its inverse.
## Returns a list containing functions get, set, getinv, and setinv.
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL # clear inverse
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Computes the matrix of the special matrix object created in makeCacheMatrix function.
## Returns either cached inverse value or solves inverse and returns if inverse
## was not solved or matrix was changed.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
