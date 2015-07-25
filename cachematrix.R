## makeCacheMatrix creates a list which can be used to cache the inverse of the input matrix x.
## cacheSolve calculates, caches and returns the inverse of the matrix x.
## If a cached value already exists, cacheSolve returns it instead.


## Takes matrix x and returns a list of four functions: set, get, setinverse and getinverse.
## "set" can be used to set a new matrix x;
## "get" retrieves x;
## "setinverse" sets a cached value for the inverse of x;
## "getinverse" retrieves the cached value if one exists.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Calculates, caches and returns the inverse of matrix x.
## If a cached value exists, the function returns it instead.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}