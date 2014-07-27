## These functions allow the user to cache the result
## of a matrix inverse operation, so that that result
## can be looked up in the cache (which is fast), rather than
## recalculated (which is slow).

## This function creates an object which can cache the
## inverse of a matrix.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## This function will return the inverse of a matrix
## that has been set up to have a cacheable inverse.
## If that inverse has already been calculated, then
## the cached inverse value will be returned. Otherwise,
## the value will be calculated.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinv()
    if(!is.null(inverse)) {
        message("Getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinv(inverse)
    inverse
}
