## Matrix inversion is costly computation and it is best cached over being computed repeatedly.
## This assignment provides codes which could cache the inverse of a matrix.

## This function creates a special "matrix" which has the capability to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      get <- function() x                                ##getting value of "matrix"
      setinverse <- function(inverse) i <<- inverse      ##setting value of inverse of the "matrix"
      getinverse <- function() i                         ##getting value of inverse of the "matrix"
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## This fuction may be used to compute inverse of "matrix" returned by code above. Although, if the inverse
## has been calculated, this function retrieves the inverse from the cache provided matrix has not changed.

cacheSolve <- function(x, ...) {
      i <- x$getinverse()
      if(!is.null(i)) {
            message("getting cached matrix")
            return(i)
      }
      matrix <- x$get()
      i <- solve(matrix, ...)
      x$setinverse(i)
      i
}
