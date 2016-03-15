## The below functions computes and caches the inverse of a matrix.

## This function caches matrix inversions. It expects a square invertible matrix as input. 
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(i) inverse <<- i
    getinverse <- function() inverse
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function computes matrix inversions. Pass the matrix from 'makeCacheMatrix' as argument
## into this function. If matrix inversion is already cached, then this function will retrieve
## the inverse from the cache.
cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if (!is.null(inverse)) {
        message("Getting cached data")
        return(inverse)
    }
    matrix <- x$get()
    inverse <- solve(matrix, ...)
    x$setinverse(inverse)
    inverse
}
