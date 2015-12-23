## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## create a function makeCacheMatrix that calculates the inverse of the
## matrix once and stores it, and gives it as output when ever required
## without calculating it again.
##
## use:
## mat <- matrix(c(11,12,15,16), nrow =2, ncol = 2)
## 
## cacheMat <- makeCacheMatrix(mat)
## cacheSolve(cacheMat)
## 
## cacheMat$set(mat)
## mat <- cacheMat$get()
##
## cacheMat$SetInverse(solve(data, ...)) ## private function holding cached inverse
## cacheMat$GetInverse()  ## private function to retrive the inverse value
makeCacheMatrix <- function(x = matrix()) {
  ## create cashe variable to save the inverse of the matrix 
  CashedInverse <- NULL
  set <- function(y) {
    x<<- y
    CashedInverse <<- NULL
  }

get <- function() x  ## fetch the value of x

## set the inverse value to the variable
SetInverse <- function(inverse) CashedInverse <<- inverse
GetInverse<- function() CashedInverse ## retrive the stored inverse value

list(set = set,
     get = get,
     SetInverse = SetInverse,
     GetInverse = GetInverse)
}

## Write a short comment describing this function
## this function returns the inverse of the matrix object

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  InvFunc <- x$GetInverse()
  
  if( !is.null(InvFunc)) {
    message("Getting cached data")
    return (InvFunc)
  }
  
  data <- x$get()
  InvFunc <- solve(data, ...) ## function containing cashed inverse of matrix
  x$SetInverse(InvFunc) ## function to retrive the cashed inverse of matrix
  InvFunc
}
