## Put comments here that give an overall description of what your
## functions do

## The method creates a special matrix (pretty much like the special vector in the example)
## the internal inverse attribute will store the inverse of the matrix. Based on its value
## we decide wether the inverse has been calculated or not.
## When data of the special matrix is changed (only possible via set function) the inverse will be set to NULL

makeCacheMatrix <- function(m = matrix()) {
  inverse <- NULL
  set <- function(y) {
    m <<- y
    inverse <<- NULL
  }
  get <- function() m
  
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## given a special matrix created by makeCacheMatrix this method 
## checks if its inverse is already calculated and so returns it
##  otherwise, it calculates it and stores it in the special matrix for futur use

cacheSolve <- function(m, ...){
  
  res <- m$getinverse()
  if(!is.null(res)) {
    message("getting cached data")
    return(res)
  }
  #Either this is the first time 
  #to calculate the inverse or the content of m was modified
  message("Need to calculate the inverse")
  data <- m$get()
  res <- solve(data, ...)
  m$setinverse(res)
  res
}
