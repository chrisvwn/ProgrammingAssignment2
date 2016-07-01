## Functions to create a matrix object and cache the inverse of the matrix
## to reduce compute time in case the inverse matrix needs to be calculated often


## This is the function that takes a matrix and creates an R object
## that includes the inverse of the matrix and functions to get and set
## the matrix and also get and set the cached inverse matrix
makeCacheMatrix <- function(x = matrix())
{
  inv <<- NULL
  
  set <- function(y)
  {
    x <<- y
    m <<- NULL
  }
  
  get <- function(){ x }
  
  setInverse <- function(inverse) { inv <<- inverse }
  
  getInverse <- function() { inv }
  
  list(set=set, get=get,
       setInverse=setInverse,
       getInverse=getInverse)
}

## This function does the actual invertion of the matrix. It checks to see if the
## inverse matrix has already been cached. If it has it returns the cached inverse
## matrix otherwise it creates the inverse matrix and then calls
## the setInverse function of the makeCacheMatrix object to set the cached inverse
## matrix
cacheSolve <- function (x, ...)
{
  inv <- x$getInverse()
  
  if(!is.null(inv))
  {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}
