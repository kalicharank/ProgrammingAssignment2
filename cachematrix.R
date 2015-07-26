# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL
   set <- function(y) {
      x <<- y
      inv <<- NULL
   }
   get <- function() return(x)
   # set the inverse value to inv in global env 
   setinv <- function(inverse) inv <<- inverse
   getinv <- function() return(inv)
   list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)

}


# This function ruturns a inverse of a matrix. If the inverse for the matrix was already calculated, 
# Then it retrives the inverse from the cache and returns it else it calculates the inverse 

cacheSolve <- function(x, ...) {
   inv <- x$getinv()
   ## check if the data is already cached, if so just return it 
   if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
   }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinv(inv)
      return(inv)
   
}
