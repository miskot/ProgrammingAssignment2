## This is a pair of functions
## that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
      x <<- y
      inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix". If the inverse has already been calculated (and the matrix has not changed), the inverse is retrived from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) return(inv)
  data <- x$get()
  inv <- solve(x, ...)
  x$setInverse(inv)
  inv
}