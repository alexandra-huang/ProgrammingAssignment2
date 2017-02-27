## The functions makeCacheMatrix and cacheSolve create matrices that can 
## cache their inverse matrices so that the inverse of the matrix does 
## not have to be calculated repeatedly and can instead be retrieve from 
## the cache

## Creates a matrix object that can cache its inverse to be used in cacheSolve
## the special matrix object is actually a list containing a function to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
    inversemat <- NULL
    setmat <- function(y) {
        x <<- y
        inversemat <<- NULL
    }
    getmat <- function() x
    setinverse <- function(solve) inversemat <<- solve
    getinverse <- function() inversemat
    list(setmat = setmat, getmat = getmat,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Computes the inverse of the matrix created by makeCacheMatrix if the inverse
## has not yet been calculated, else gets the already cached inverse 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inversemat <- x$getinverse()
    if(!is.null(inversemat)) {
        message("getting the cached inverse of the matrix")
        return(inversemat)
    }
    data <- x$getmat()
    inversemat <- solve(data, ...)
    x$setinverse(inversemat)
    inversemat
}
