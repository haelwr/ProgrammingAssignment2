## creates a special matrix object 
## that can cache its inverse
## sets the value of the matrix 
## gets the value of the matrix
## sets the value of the matrix inverse
## gets the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}

## calculates the inverse based on the above the function
## if the inverse has already been calculated (and not changed)
## then cachesolve should retrieve the inverse from the cache

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
