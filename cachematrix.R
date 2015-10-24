## cachematrix.R
## Functions makeCacheMatrix and cacheSolve work together to create
## a mechanism to retrieve the cached result of a matrix inversion, if the
## result already exists.  If not, the matrix inversion will be calculated
## and returned.

## makeCacheMatrix creates a list of functions which are subsequently
## called to set or get the values of the initial matrix or its inverse.
## The input for makeCacheMatrix is a square invertable matrix
## The output is a list of functions

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y        # Sets new matrix
                inv <<- NULL   # Sets inv to NULL when setting new matrix
        }
        get <-function() x                               # Gets matrix
        setinverse <- function(inverse) inv <<- inverse  # Sets inv
        getinverse <- function() inv                     # Gets cached inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve returns the cached value "inv" of the inverted matrix if it exists.
## If the value doesn't exist, cacheSolve performs the inversion calculation on 
## the matrix input from makeCacheMatrix using the R function solve.
## The input for cacheSolve is the list produced by makeCacheMatrix
## The output is either the cached value of inv and a message, or a freshly
## calcuated value.

cacheSolve <- function(x, ...) {
        # assign the cached value of inv to inv
        inv <- x$getinverse()
        # If inv value is not null, return message and inv
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()          # Get matrix data
        inv <- solve(data, ...)  # Use solve to calculate matrix inversion
        x$setinverse(inv)        # Set inv to calculated value
        inv                      # Return inv
}

## Example Usage:
## Source cachematrix.R
## Create matrix x
## Execute cachefunctionlist <- makeCacheMatrix(x)
## Execute cacheSolve(cachefunctionlist)
## Matrix inversion will be calculated
## Execute cacheSolve(cachefunctionlist) a second time
## Cached value will be returned with "getting cached data" message

## Note that invoking cacheSolve(makeCacheMatrix(x)) will initialize inv and fail
## to find a cached value.

