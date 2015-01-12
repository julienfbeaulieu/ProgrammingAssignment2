##makeCacheMatrix and cacheSolve is a set of two functions allowing
##to calculate the inverse of a square matrix and cache the result 
##for later use

##makeCacheMatrix expands the atomic vector x into a list array
##containing its value and the value of its inverse. Initially,
##the inverse 's' is set to NULL.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
      x <<- y
      s <<- NULL
  }
  
  get <- function() x
  setSolve <- function(solve) s <<- solve
  getSolve <- function() s
  list(set = set, get = get, 
       setSolve = setSolve, 
       getSolve = getSolve)
  
}

##cacheSolve expands on the the 'solve' function to add a cache 
## component. When called the first time, it returns the inverse
## of x and stores the answer with 'setSolve'. On a second call,
## the function simply returns the cached value.

cacheSolve <- function(x, ...) {
  s <- x$getSolve()
  if(!is.null(s)) {
      message("getting cached data")
      return(s)
  }
  data <- x$get()
  s <- solve(data,...)
  x$setSolve(s)
  s  
}