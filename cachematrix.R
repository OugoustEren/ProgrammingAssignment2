#Assignment: Caching the Inverse of a Matrix


## Create a Square Matrix

makeCacheMatrix <- function(x = matrix()) {
  
  #Computing a square matrix that has inverse
  
  ind <- NULL
  set <- function(y) {
    x <<- y
    ind <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) ind <<- inverse
  getinverse <- function() ind
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}




##Solve Inverse Matrix function

cacheSolve <- function(x, ...) {
  
  # Return a matrix that is the inverse of 'x
  
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

# TESTING
## With pre-existing function in R
B <- matrix(c(1,2,3,4),2,2)  
solve(B)

## With my functions
B1 <- makeCacheMatrix(B)
cacheSolve(B1)
