## These functions work together to cache the inverse of a matrix.
## This can save computation time when the inverse is needed repeatedly
## but the matrix does not change between calls.

## This function creates a special "matrix" object that can cache its inverse.
## It returns a list of functions to set and get the matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL  # Reset the cached inverse when a new matrix is set
    }
    get <- function() x  # Return the matrix
    setinverse <- function(inverse) inv <<- inverse  # Cache the inverse
    getinverse <- function() inv  # Return the cached inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then it retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message("getting cached data")  # Inform the user that cached data is being used
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)  # Compute the inverse
    x$setinverse(inv)        # Cache the computed inverse
    inv                      # Return the inverse
}

