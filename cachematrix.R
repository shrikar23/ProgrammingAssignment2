## The Function makeCacheMatrix creates a list containing four functions as below
## 1. To set the value of the matrix (set())
## 2. To get the value of the matrix (get())
## 3. To set the value of the inverse (setinverse())
## 4. TO get the value of the inverse (getinverse())
## the function cacheSolve calculates the inverse of a matrix using the functions created
## by makeCacheMatrix. cacheSolve first checks if the inverse has already been 
## calculated. If so, it gets the inverse form the cache and skips the computation.
## Otherwise, it computes the inverse of the matrix and sets the value of inverse in
## cache via thesetinverse function.

## This function creates a list of four functions.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  ## This function sets the value of the matrix
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  ## This function gets the value of matrix
  get <- function() x
  ## This function sets the value of inverse
  setinverse <- function(inverse) inv <<- inverse
  ## This function gets the value of inverse
  getinverse <- function() inv
  list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function calculates the inverse of the matrix with the help of functions created
## in makeCacheMatrix.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  ## checks if the inverse is present in cache.
  if(!is,null(inv)){
    message("getting cache data")
    return(inv)
  }
  data <- x$get()
  ## calculating the inverse of 'x'
  inv <- solve(data,...)
  x$setinverse(inv)  
  ## Return a matrix that is the inverse of 'x'
  inv
}
