## Performance-enhancing wrapper for matrices, providing caching of the result of solve()

## makeCacheMatrix(x)
## Parameters
##  x: matrix
## Returns:
##  list containing setters and getters for the matrix and its inverse
## Remarks:
##  * setting the matrix value will also clear the inverse
##  * the inverse is initialized to NULL.

makeCacheMatrix <- function(x = matrix()) {
  cachedInverse <- NULL
  
  # set/get the matrix value
  set <- function(value) {
    x <<- value
    cachedInverse <<- NULL
  }
  get <- function() x
  
  # set/get the inverse value
  setinverse <- function(inverse) cachedInverse <<- inverse
  getinverse <- function() cachedInverse
  
  # return the public interface
  list(set = set, get = get,
       setinverse = setinverse, getinverse = getinverse
       )
}


## cacheSolve(x)
## Parameters
##  x: object created from makeCacheMatrix
##     (see this function for the description of the provided interface)
## Returns:
##  The inverse of (the matrix value of) x.
## Remarks:
##  The first time the function is called on x, it will calculate the inverse of x and cache it.
##  Subsequent calls to the function will retrieve the cached result.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
