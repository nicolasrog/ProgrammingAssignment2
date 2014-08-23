## These two functions create objects that can be cached,
## specifically a matrix and its inverse, and allow to calculate the
## inverse of the matrix and to cache it if it hasn't been made yet

## This function creates a list of objects that can be cached, 
## including the original matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

## This function retrieves the result of the inverse of the matrix 
## when it is in the cache
## In the case the result of the inverse is not in the cache
## the function calculates it and put it in the cache 
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  m      
}
