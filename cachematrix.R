## Put comments here that give an overall description of what your
## functions do
## makeCasheMatrix and cacheSolve
## Functions do the following ...

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inverseV <- NULL
        set <- function(y) {
                x <<- y
                inverseV <<- NULL
        }
        get <-function() x
        setinverse <- function(inverseF) inverseS <<- inverseF
        getinverse <- function() inverseV
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverseV <- x$getinverse()
        if(!is.null(inverseV)) {
                message("getting cached data")
                return(inverseV)
        }
        data <- x$get()
        inverseV <--inverseF(data, ...)
        x$setinverse(inverseV)
        inverseV
}
