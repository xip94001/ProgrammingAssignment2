## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Function makeCacheMatrix creates a special "matrix", which is really a list 
## containing functions to 
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse matrix
## 4.get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    invm <- NULL
    set <- function(y) {
      x <<- y
      invm <<- NULL
    }
    get <- function() x
    setinversematrix <- function(inversematrix) invm <<- inversematrix
    getinversematrix <- function() invm
    list(set = set, get = get,
       setinversematrix = setinversematrix,
       getinversematrix = getinversematrix)
}


## Write a short comment describing this function

## The following function calculates the inverse of the special "matrix" created with 
## the above function. However, it first checks to see if the inverse matrix has already 
## been calculated. If so, it gets the inverse matrix from the cache and skips the 
## computation. Otherwise, it calculates the inverse matrix of the data using solve and 
## sets the value of the inverse matrix in the cache via the setinversematrix function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
  
    invm <- x$getinversematrix()
    if(!is.null(invm)) {
      message("getting cached data")
      return(invm)
    }
    data <- x$get()
    invm <- solve(data, ...)
    x$setinversematrix(invm)
    invm
  
}
