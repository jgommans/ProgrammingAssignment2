## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set_matrix <- function(y) {
            x <<- y
            m <<- NULL
      }
      get_matrix <- function() x
      setinverse <- function(inverse_matrix) m <<- inverse_matrix
      getinverse <- function() m
      list(set_matrix = set_matrix, get_matrix = get_matrix,
           setinverse = setinverse,
           getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      m <- x$getinverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get_matrix()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}
