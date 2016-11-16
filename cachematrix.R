##the following two functions allow to caching the inverse of a matrix rather than 
## compute it repeatedly

## the first function makeCacheMatrix creates a list containing a function to
## set and get the value of the matrix and set and get the valute of the 
## inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) inv <<- solve
    getsolve <- function() inv
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
  
}


## the following function calculates the inverse of the matrix created with the
## above function, but before it checks to see if the mean has already been 
## calculated: if it gets the inverse matrix from the cache, it skips 
## the computation; otherwise it calculates the inverse of the matrix and sets the 
## inverse matrix in the cache through the setsolve function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
    inv <- x$getsolve()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setsolve(inv)
    inv
  
}
