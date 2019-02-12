### These functions provide an inverted matric from the cache 
### to prevent duplicating time-consuming matrix computations to find
### the inverse matrix of an invertible matrix. 

## Creates a list that represents a cacheable matrix

makeCacheMatrix <- function(x = matrix()) {
  minverse<-NULL
  set <- function(y) {
    x <<- y
    minverse <<-NULL
  }
  get<- function() x
  setinverse <- function(minv) minverse <<- minv
  getinverse <- function() minverse
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Provides an inverse matrix from the cache or performs the matrix computations

cacheSolve <- function(x, ...) {
  minverse <- x$getinverse()
  if(!is.null(minverse)) {
    message("getting cached inverse")
    return(minverse)
  }
  data <- x$get()
  minverse <- solve(data)
  x$setinverse(minverse)
  minverse
  ## Return a matrix that is the inverse of 'x'
}
