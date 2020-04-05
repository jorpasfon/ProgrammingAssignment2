## Functions to cache the inverse of a matrix

## Matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  ## Inverse matrix variable initialized
  inverseMatrix <- NULL
  
  ## Method to set the matrix
  set <- function(matrix) {
    x <<- matrix
    inverseMatrix <<- NULL
  }
  
  ## Method to get the matrix
  get <- function() x
  
  ## Method to set the inverse of the matrix
  setInverse <- function(inverse) inverseMatrix <<- inverse
  
  ## Method to get the inverse of the matrix
  getInverse <- function() inverseMatrix
  
  ## The function returns a list with its methods
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Return the inverse of a "makeChacheMatrix" matrix if it is cached, or
## compute it and cache it otherwise

cacheSolve <- function(x, ...) {
  ## Get the stored matrix inverse
  inverseMatrix <- x$getInverse()
  
  ## If exists on cache, return it
  if(!is.null(inverseMatrix)) {
    message("Getting cached inverse matrix")
    return(inverseMatrix)
  }
  
  ## If inverse not in cache, get the matrix 
  matrix <- x$get()
  
  ## Compute the inverse of the matrix and cache the result
  inverseMatrix <- solve(matrix)
  x$setInverse(inverseMatrix)
  
  ## Return the inverse of the matrix
  inverseMatrix
}
