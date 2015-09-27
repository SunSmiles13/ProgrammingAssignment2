## Assignment 2
## Function to cache and inverse the matrix - only for square matrix and only for invertible matrixes
## 

## makeCacheMatrix function creates special matrix object and cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve function computes the inverse of a matrix if its not in cache else returns the inverse from the cache

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if (!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- mean(data, ...)
  x$setInverse(i)
  i
}

