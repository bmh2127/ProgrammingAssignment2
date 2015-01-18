## the makeCacheMatrix function creates special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) { ## sets x equal to an empty matrix
  Inv <- NULL ## set inverse equal to NULL
  set <- function(y) {
    x <<- y ## set function assigns y argument equal to x
    Inv <<- NULL ## reset of inverse to Null
  }
  get <- function() x ## return the matrix
  setInv <- function(solve) Inv <<- solve ## setInv overrides the NULL value of Inv to return inverse matrix
  getInv <- function() Inv ## returns inverse matrix
  list(set = set, get = get, setInv = setInv, getInv = getInv) ## creates list of the functions
} 
## cacheSolve function returns the calculated inverse matrix, and if not already calculated, calculates the inverse of the new matrix
cacheSolve <- function(x, ...) {
  Inv <- x$getInv() ## gets the most recent value for the inverse
  if(!is.null(Inv)) { ## if the value of the inverse is not null, returns the already calculated inverse of matrix x
    message("getting cached data")
    return(Inv)
  }
  data <- x$get() ## if the value of the inverse is NULL, calculates the inverse of matrix x
  Inv <- solve(data, ...)
  x$setInv(Inv) ## sets inverse to newly calculated inverse of matrix x
  Inv ## returns inverse matrix
}
