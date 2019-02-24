## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix is the function to create a special matrix object that can cache its inverse
## cacheSolve is the function computes the inverse of the special matrix returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve would
## retrieve the inverse from the cache. Here we assume that the matrix x would be squared (2-by-2) 
## invertible matrix, whose inverse can be done by solve(x)

## Write a short comment describing this function

## The function makeCacheMatrix creates a special "matrix", which is really a list containing a function to 
## 1. set the value of the matrix
## 2. get the value of the matrix 
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    I = NULL
    setmatrix <- function(y) {
        x <<- y
        I = NULL
    }
    getmatrix <- function() x
    setinverse <- function(inverse) I <<- inverse
    getinverse <- function() I
    list(setmatrix = setmatrix,
         getmatrix = getmatrix,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function

## The following function, cacheSolve, calculates the inverse of the special matrix created by the above function.
## However, it first checks to seee if the inverse has already been calculated. If so, it gets the inverse from 
## the cache and skips the compuation. Otherwise, it calculates the inverse of the data and sets the value of the 
## inverse matrix in cach via the setinverse function.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    I <- x$getinverse()
    if(!is.null(I)) {
        message("getting cached data")
        return(I)
    }
    data <- x$getmatrix()
    I <- solve(data, ...)
    x$setinverse(I)
    I
}
