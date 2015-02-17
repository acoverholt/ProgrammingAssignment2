## The following pair of functions are used to invert matrices. 
## As matrix inversion is computationally intensive, the function cacheSolve will
## check the cache created in makeCacheMatrix for a previously computed solution.
## If previous solution is found, it will return that solution and not "re-solve".
## 
## Created by Andrew Overholt on 2/16/15

## Creates a list containing four functions as described in the function body
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL                 ##variable used to store the solution
  set <- function(y) {      ##First function: sets the matrix variable to the given matrix
    x <<- y
    m <<- NULL              ##Resets solution to NULL (clearing cache), triggering solve
  }                         ##(Note: will now "re-solve" matrix even if matrix is identical to previous use)
  get <- function() x       ##Second function: retrieves the matrix
  setinverse <- function(solve) m <<- solve   ##Third function: sets the inverse 
  getinverse <- function() m    ##Fourth function: retrieves the inverse from cache
  list(set = set, get = get,    ##Creates a list out of the 4 functions
       setinverse = setinverse,
       getinverse = getinverse)
}


## Returns the inverse of the given "matrix", however only solves it if the solution is not stored in the cache
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()     ##Attempting to retrieve inverse from cache
  if(!is.null(m)) {       ##Checks to see if inverse exists in cache
    message("getting cached data")
    return(m)             ##Returns cached inverse (exiting from function)
  }
  data <- x$get()         ##Retrieves the special matrix
  m <- solve(data, ...)   ##Finds the inverse of the special matrix
  x$setinverse(m)         ##Sets the cache to the computed inverse
  m                       ##Return computed inverse
}
