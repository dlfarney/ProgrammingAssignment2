## Assignment: Caching the Inverse of a Matrix
##
## Assumed : The matrix supplied is always invertible.
##
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than computing it 
## repeatedly. This assignment is to write a pair of functions that cache
## the inverse of a matrix.
## 
## makeCacheMatrix: 
## This function creates a special "matrix" object that can cache its 
## inverse.
##
## cacheSolve: 
## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix. If the inverse has already been calculated
## (and the matrix has not changed), then cacheSolve should retrieve the 
## inverse from the cache.
##

##
## The first function, `makeCacheMatrix` creates a special "matrix" object, 
## which is really a list containing a function to
##  
## 1.  set: sets the value of the original matrix
## 2.  get: gets the value of the original matrix
## 3.  setinverse: sets the value of the inverse matrix
## 4.  getinverse: gets the value of the inverse matrix 
##
##
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, 
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}

##
## The following function cacheSolve calculates the inverse of the special 
## "matrix" created with the above function. 
## However, it first checks to see if the inverse of the matrix has already
## been calculated. If so, it gets the inverse from the cache and skips 
## the inversion. Otherwise, it calculates the inverse of the matrix and 
## sets the value of the inverse in the cache via the `setinverse` function
## and return the inverse matrix.
##
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("Getting Cached Data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
