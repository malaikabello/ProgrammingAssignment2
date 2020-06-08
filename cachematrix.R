## This program consists of two function:
## the first, makeCacheMatrix helps in creating a matrix that can cache its inverse
## the second, cacheSolve returns the inverse of the original matrix


## this function helps in creating a matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    z <- NULL
  set <- function(y) {
          x <<- y
          z <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) z <<- inverse
  getinverse <- function() z
  list(set = set,get = get,setinverse = setinverse,getinverse = getinverse)
}

## this function computes the inverse of the matrix returned by makeCacheMatrix above
cacheSolve <- function(x, ...) {
  z <- x$getinverse()
  if (!is.null(z)) {
          message("getting cached data")
          return(z)
  }
  data <- x$get()
  z <- solve(data, ...)
  x$setinverse(z)
  z
}
