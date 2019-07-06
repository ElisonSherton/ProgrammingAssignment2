## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Creates a matrix with 4 different functionalities
## Get and set the value of a matrix
## Get and set the inverse of the values of a matrix

makeCacheMatrix <- function(x = matrix()) {
  invM <- NULL
  set <- function(y) {
    x <<- y
    invM <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) invM <<- inverse
  getinv <- function() invM
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
## Caches the inverse of a matrix if the matrix hasn't changed 
## and it's inverse is pre-computed
## Calculates the inverse of a matrix otherwise.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinv()
    if(!is.null(inverse) & identical(x,x$get())) {
      message("getting cached data")
      return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinv(inverse)
    inverse
}
