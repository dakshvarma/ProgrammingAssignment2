## This is a pair of functions that calculate and store the inverse of a matrix
## and a wrapper function that returns the pre calculated inverse
## if it has already been called

## Sets up the matrix inverse cache
makeCacheMatrix <- function(x = matrix()) {
  matrixInverse <- NULL
  set <- function(y) {
    x <<- y
    matrixInverse <<- NULL
  }
  get <- function() x
  setMatrixInverse <- function(inverse) matrixInverse <<- inverse
  getMatrixInverse <- function() matrixInverse
  list(set = set, get = get,
       setMatrixInverse = setMatrixInverse,
       getMatrixInverse = getMatrixInverse)
}


## wrapper function that returns cached inverted matrix if pre calculated
## otherwise recalculates
cacheSolve <- function(x, ...) {
  matrixInverse <- x$getMatrixInverse()
  if(!is.null(matrixInverse)) {
    message("getting cached data")
    return(matrixInverse)
  }
  data <- x$get()
  matrixInverse <- solve(data)
  x$setMatrixInverse(matrixInverse)
  matrixInverse
}
