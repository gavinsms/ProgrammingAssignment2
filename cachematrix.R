## Creates a special matrix object whose inverse is cached.
## Once the solution is cached, the second function checks the cache before attempting to solve again.

## Create a special matrix object that can cache its solution.

makeCacheMatrix <- function(x = matrix()) {

  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinv <- function(inv) s <<- inv
  getinv <- function() s
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
  }


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getinv()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinv(s)
  s
}
