## Put comments here that give an overall description of what your
## functions do

## A function the keeps an inverse matrix in its cahce ('m').
## It returns a list of possible functions to call to set and get the matrix end the cached inverse matrix.

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


## A function that checks if there is already an inverse matrix cached (in 'makeCacheMatrix') and if so returns the cache.
## If cache is empty it inverses the matrix and sets it in the cache

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      m <- x$getinverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get_matrix()
      m <- solve(data)
      x$setinverse(m)
      m
}
