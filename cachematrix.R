## These two functions create and cache the inverse of a matrix

## A function to create a "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    get <- function() x
    setInverse <- function(Inverse) inverse <<- Inverse
    getInverse <- function() inverse
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## A function to compute the inverse of the matrix returned by makeCacheMatrix. Returns inverse if already calculated.

cacheSolve <- function(x, ...) {
        inverse <- x$getInverse()
        
        if(!is.null(inverse)) {
            message("getting cached data")
            return(inverse)
        }
        
        matrix <- x$get()
        inverse <- solve(matrix, ...)
        x$setInverse(inverse)
        inverse
        
}
