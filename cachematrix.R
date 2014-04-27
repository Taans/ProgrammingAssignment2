## functions do

## function makeCacheMatrix() Creates special "matrix" which is a list store cache of matrix x

makeCacheMatrix <- function(x = matrix()) {
  inv<- NULL
  set <- function(y) {
    x<<-y
    inv<<-NULL
  }
  get <- function() x
  setInv <- function(solve) inv <<-solve
  getInv <- function() inv
  list(set=set, get=get, setInv=setInv, getInv=getInv)
}
## function cacheSolve() check if the cached inverse matrix existing return it, if not, calculate
## it, store it

cacheSolve <- function(x, ...) {
  inv<- x$getInv()
  if (!is.null(inv)) {
    message("Getting cached data")
    return(inv)
  }
  myMatrix <- x$get()
  inv<-solve(myMatrix)
  x$setInv(inv)
  inv
  ## Return a matrix that is the inverse of 'x'
}
