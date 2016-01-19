## Put comments here that give an overall description of what your
## functions do


## Write a short comment describing this function

# The function makeCacheMatrix creates a matrix that contains a function to cache it's inverse, by doing the following:
# - set the value of the matrix that we will later use to compute its inverse (in the next makeCacheMatrix function)
# - get the value of that matrix
# - set the value of the inverse of the matrix
# - get the value of the inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) 
{
  myinverse <- NULL
  set <- function(y) {
    x <<- y
    myinverse <<- NULL
  }
  get <- function() x
  setmyinverse <- function(inverse) myinverse <<- inverse
  getmyinverse <- function() myinverse
  list(set = set, get = get,
       setmyinverse = setmyinverse,
       getmyinverse = getmyinverse)
  
  
}


## Write a short comment describing this function
# This cacheSolve function computes the inverse of a matrix returned by the makeCacheMatrix above,if not already computed, 
# otherwise it just retreives the cached inverse value of the matrix.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  myinverse <- x$getmyinverse()
  if(!is.null(myinverse)) {
    message("getting cached data")
    return(myinverse)
  }
  data <- x$get()
  myinverse <- solve(data, ...)
  x$setmyinverse(myinverse)
  myinverse
  
}
