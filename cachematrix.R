#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL # set to NULL initially
  set <- function(y) {
    x <<- y
    i <<- NULL # reset to NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(setmatrix = setmatrix, getmatrix = getmatrix, setInverse = setInverse, getInverse = getInverse)
}



# cacheSolve: This function computes the inverse of the 
# special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve 
# should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  # calculate the inverse of x
  i <- x$getInverse()
  if (!is.null(i)) {
    return(i)
  }
  val_matrix <- x$get()
  i <- solve(val_matrix, ...)
  x$setInverse(i)
  i
}
