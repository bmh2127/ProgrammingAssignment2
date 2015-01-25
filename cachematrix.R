##
## These set of functions create a special matrix to cache the inverse of a matrix
## 

## makeCacheMatrix receives a matrix as an input, and creates a special matrix
## to cache the inverse value. It has functions for storing the value of the
## inverse, providing the value to the other function if available, and storing
## the value if passed by 'cacheSolve' function
##
makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
   
    ## Set values of matrix (x) and inverse matrix (inv) to cache
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    ## Returns matrix stored in cache
    get <- function() x
    
    ## Sets value of inverse matrix in cache
    setinv <- function(inverse) inv <<- inverse
  
    ## Returns inverse value of matrix
    getinv <- function() inv
    
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function calculates the inverse of the special matrix created by
## makeCacheMatrix. It first checks to see of the the inverse has already
## been calculated. If so, it `get`s the inverse from the cache and skips the
## the computation. Otherwise, it calculates the inverse of the matrix and sets
## the value of the inverse in the cache via the `setinv` function
##
cacheSolve <- function(x, ...) {
  ## Get the inverse value of the matrix from cache
  m <- x$getinv()
  
  ## If inverse value exists, return it and exit the function
  if(!is.null(m)) {
    message("Getting cached data")
    
    return(m)
  }
  
  ## If inverse is not cached, get the matrix value from cache, calculate the
  ## inverse, then store in cache using 'setinv' function
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
  
}
