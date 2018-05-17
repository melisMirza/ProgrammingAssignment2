## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  get <- function() x
  set <- function(mNew){
    x <<- mNew
    inv <<- NULL
  }
  setInv <- function(inved) inv <<- inved
  getInv <- function() inv
  list(get = get,set = set, setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  dat <- x$get()
  inv <- solve(dat,...)
  x$setInv(inv)
  inv
}
