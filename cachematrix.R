## Matrix inversion is usually a costly computation and there may be some benefit to caching
## the inverse of a matrix rather than computing it repeatedly.
## The following functions cache the inverse of a matrix.

## The first function,  'makeCacheMatrix', creates a special "matrix" object that can cache its inverse.
## Essentially, it is a list containing a function to

# 1.  set the value of the matrix
# 2.  get the value of the matrix
# 3.  set the value of the inverse
# 4.  get the value of the inverse

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


## The second function, 'cacheSolve', computes the inverse of the special "matrix" returned by `makeCacheMatrix` above.
## If the inverse has already been calculated (and the matrix has not changed), then `cacheSolve` should retrieve
## the inverse from the cache and skip the computation. Otherwise, it computes the inverse of the data and sets
## the value of the inverse in the cache via the 'setinverse' function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
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
