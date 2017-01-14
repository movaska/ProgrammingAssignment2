## The functions in this file allow the user to cache the inverse of a matrix
## so that it does not need to be recomputed.
##
## makeCacheMatrix() creates an object where the matrix and its inverse are
## stored.
##
## cacheSolve() returns the inverse matrix by either getting it from the cache
## or solving it if necessary.



## When given a matrix as input, returns a list of functions
## for getting and setting the matrix and its inverse.
## If inverse has not been computed, getinverse() returns NULL.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## When given a CacheMatrix object x created with makeCacheMatrix() as input,
## returns the inverse of the matrix stored in x.
## If inverse has already been cached in x, the inverse is taken from the cache.
## Otherwise cacheSolve() calls solve() to get the inverse and stores it in x. 

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
