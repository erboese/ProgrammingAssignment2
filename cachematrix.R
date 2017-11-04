## Programming Assignment #2 - Week 3 R Programming 
## File creates two functions - the first creates a 'matrix' object which can cache
## the inverse of the cache.  The second function calculates the inverse of the matrix.  
## If the inverse has already been calculated the function just returns that calculation.   

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function (y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  ## create list to return activities on matrix
  list(set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Calculates the inverse of a matrix

cacheSolve <- function(x, ...) {
  inv<-x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data<-x$get()   ## get the matrix
  inv <- solve(data) ## create the inverse
  x$setinverse <- inv  ## save the inverse
  inv
}
