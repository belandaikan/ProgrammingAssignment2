## A pair of function to create a matrix object and to
## compute or retrieve its matrix, the latter if it has
## calculated before

## This function creates a matrix object and is list of 
## four other functions which can be accessed through
## the main function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of a matrix once
## using the solve() function. If the inverse has been 
## calculated, it is stored in a cache and retrieved
## next time the function is called on the same matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
