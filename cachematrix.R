## Functions to create a matrix object that can 
## calculate and store the cached inverse.

## makeCacheMatrix takes a matrix that is invertable as a parameter. 
## It returns a list of functions: 
## set - takes a matrix as a parameter and caches it. 
## get - returns the cached matrix
## getInverse - returns the current cached inverse matrix
## setInverse = sets the inverse
makeCacheMatrix <- function(x = matrix()) {
  cacheInverse <<- NULL
  set <- function(m){
    x <<- m
    cacheInverse <<- NULL  ## new matrix, so remove old value
  }
  get <- function() x  ## get current matrix of this object
  setInverse <<- function(invrs) cacheInverse <<- invrs  ##inverse is calculated in calling function 
  getInverse <- function() cacheInverse 
  list(set = set, get = get, setInverse = setInverse, 
       getInverse = getInverse) ## return list of functions this object provides
  
}

## cacheSovle takes as a parameter a matrix object returned from makeCacheMatrix and 
## returns the inverse of the matrix contained in that object.
cacheSolve <- function(x, ...) {
  mtrxInverse <- x$getInverse()
  if(!is.null(mtrxInverse))  ##inverse has already been calculated
  {
    return(mtrxInverse)
  }
  
  ##need to calculate inverse
  mtrx <- x$get()
  mtrxInverse <- solve(mtrx)
  x$setInverse(mtrxInverse) ## cache the inverse
  mtrxInverse ## Return a matrix that is the inverse of matrix in object x
}
