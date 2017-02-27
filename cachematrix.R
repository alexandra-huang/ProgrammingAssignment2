## The functions makeCacheMatrix and cacheSolve create matrices that can 
## cache their inverse matrices so that the inverse of the matrix does 
## not have to be calculated repeatedly and can instead be retrieve from 
## the cache

## Creates a "matrix" object that can cache its inverse to be used in 
## cacheSolve
## the special matrix object is actually a list containing a function to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
    # the inverse of the matrix 
    inversemat <- NULL
    
    # sets the value of the matrix
    setmat <- function(y) {
        x <<- y
        inversemat <<- NULL
    }
    
    # gets the value of the matrix
    getmat <- function() x
    
    # sets the inverse matrix to the result of calling the solve function
    setinverse <- function(solve) inversemat <<- solve
    
    # gets the inverse matrix
    getinverse <- function() inversemat
    
    # returns the list object with the four functions
    list(setmat = setmat, getmat = getmat,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Computes the inverse of the matrix object created by makeCacheMatrix 
## if the inverse has not yet been calculated, 
## else gets the already cached inverse 

cacheSolve <- function(x, ...) {
    inversemat <- x$getinverse()
    
    # retrieves the inverse of the matrix if it already exists in cache
    if(!is.null(inversemat)) {
        message("getting the cached inverse of the matrix")
        return(inversemat)
    }
    data <- x$getmat()
    
    # calculates the inverse of the matrix if it doesn't yet exist
    inversemat <- solve(data, ...)
    x$setinverse(inversemat)
    inversemat
}
