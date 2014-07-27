## These functions allow the user to cache the result
## of a matrix inverse operation, so that that result
## can be looked up in the cache (which is fast), rather than
## recalculated (which is slow).
##
## This code is based on the mean-caching code which was
## provided with this assignment.

## This function creates an object which can cache the
## inverse of a matrix.
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinv <- function(inverse_in) inverse <<- inverse_in
    getinv <- function() inverse
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## This function will return the inverse of a matrix
## that has been set up to have a cacheable inverse
## using the previous function.
##
## If that inverse has already been calculated, then
## the cached inverse value will be returned. Otherwise,
## the value will be calculated and cached.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    cached_inverse <- x$getinv()
    if(!is.null(cached_inverse)) {
        message("Getting cached data")
        return(cached_inverse)
    }
    matrix <- x$get()
    inverse <- solve(matrix, ...)
    x$setinv(inverse)
    inverse
}
