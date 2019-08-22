## The following two functions allow you to cache the inverse of a matrix

## Create a matrix that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL
   set <- function(y) {
      x <<- y
      inv <<- NULL
   }
   get <- function() x
   setInverse <- function(inverse) inv <<- inverse
   getInverse <- function() inv
   list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}

## Calculate the inverse of the matrix or retrieve it from cache

cacheSolve <- function(x, ...) {
   inv <- x$getInverse()
   if (!is.null(inv)) {
      message("getting cached matrix")
      return(inv)
   }
   mat <- x$get()
   inv <- solve(mat, ...)
   x$setInverse(inv)
   inv
}
