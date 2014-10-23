## The cacheSolve function inverses a matrix and caches it in makeCacheMatrix.


## A function the keeps an inverse matrix in its cahce ('m').
## It returns a list of possible functions to call to set and get the matrix end the cached inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
      cached_matrix <- NULL
      
      ## This set_matrix function is not needed but is here for convenience.
      ## When needed uncomment and add set_matrix to the list that is returned.      
      #set_matrix <- function(matrix_data) {
      #      x <<- matrix_data
      #      cached_matrix <<- NULL
      #}      
      get_matrix <- function() x
      
      ## Functions to store and get an inversed matrix
      setinverse <- function(inverse_matrix) cached_matrix <<- inverse_matrix
      getinverse <- function() cached_matrix
      
      ## Return a list with functions that can be used
      list(get_matrix = get_matrix,
           setinverse = setinverse,
           getinverse = getinverse)
}


## A function that checks if there is already an inverse matrix cached (in 'makeCacheMatrix') and if so returns the cache.
## If cache is empty it inverses the matrix and sets it in the cache

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      ## First check for the cache
      inverse_matrix <- x$getinverse()
      if(!is.null(inverse_matrix)) {
            message("getting cached data")
            return(inverse_matrix)
      }
      ## If there is no cache than inverse the matrix and store it in the cache
      data <- x$get_matrix()
      inverse_matrix <- solve(data)
      x$setinverse(inverse_matrix)
      
      ## Return the inverse matrix
      inverse_matrix
}
