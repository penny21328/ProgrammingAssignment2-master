## This s an R function which is able to cache potentially time-consuming computations.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix(),...) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix above. 


cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

x <-makeCacheMatrix(matrix(c(1,2,3,2,1,2,3,2,1),ncol=3,nrow=3))
cacheSolve(x)