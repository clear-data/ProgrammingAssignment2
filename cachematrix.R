## cachematrix.R
## Functions makeFunctionVector and cacheSolve work together to create
## a mechanism to retrieve the cached results of a matrix inversion, if the
## results already exist.  If not, the matrix inversion will be calculated
## and returned.
##  
## Program assumes input matrix x is a square invertable matrix

## makeCacheMatrix creates a list of functions which can be called individually
## to set or get the values of the initial matrix or its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <-function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve returns the cached value of the matrix "inverse" if it exists
## if the value doesn't exist, cacheSolve does the inversion calculation using
## the R function solve.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inverse <--solve(data, ...)
        x$setinverse(inv)
        inv
}
