## The makeCacheMatrix creates a special "matrix" object that can cache its inverse. 
## cacheSolve takes a makeCacheMatrix object as input. If the inverse has already been 
## calculated, then the cachesolve should retrieve the inverse from the cache.

## The makeCacheMatrix creates a vector of functions that could cache a copy of itself 
## ,calculate its inverse and cache it inside.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function() m <<- solve(x)
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve takes a makeCacheMatrix object as input. If the inverse has already been 
## calculated, then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse()
  m
  
}
