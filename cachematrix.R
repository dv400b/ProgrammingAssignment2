### Functions for manipulating "cached matrix objects," or "CMOs" for short.

#Define methods (embedded functions) that apply to CMOs:
#  $set:  assign matrix value
#  $get:  return matrix value
#  $setInverse:  set inverse matrix value [DO NOT call directly, call cacheSolve() instead] 
#  $getInverse:  return inverse matrix value
makeCacheMatrix <- function(x = matrix()) {
   xinv <- NULL
   set <- function(y) {
      x    <<- y
      xinv <<- NULL
   }
   get <- function() x
   setinverse <- function(matr) xinv <<- matr
   getinverse <- function() xinv
   list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}

## Return the inverse of a CMO.
# If the CMO contains a cached inverse matrix value, return cached value.
# Otherwise, calc the inverse matrix, populate the CMO inverse matrix value, and return calced value.
cacheSolve <- function(x, ...) {
   xinv <- x$getinverse()
   if(!is.null(xinv)) {
      message("getting cached data")
      return(xinv)
   }
   data <- x$get()
   xinv <- solve(data, ...)
   x$setinverse(xinv)
   xinv
}
