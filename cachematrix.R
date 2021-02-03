## makes a Special Vector containing a function to set a matrix
## in it's own environment, and solving it by findings it's inverse if not
## already stored in cache.

## makes a Special Vector containing four functions that get, set, and get inverse
## set inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) m <<- inv
  getinverse <- function() m
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Returns a matrix that is the inverse of matrix created in makeCacheMatrix
## if not already stored in cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)){
    message("Getting cached data")
    return(m)
  }
  data <- x$get()
  m <- inv(data,...)
  x$setinverse(m)
  m        ## Return a matrix that is the inverse of 'x'
}
