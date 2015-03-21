## Utility functions to cache and access a repeatedly used inverse matrix
## using lexical scoping

makeCacheMatrix <- function(x = matrix()) {
    # Creates list of accessor functions to matrix x and its cached inverse.
    # 
    # Args:
    #    x: a square matrix
    # Returns:
    #    List of accessor functions set, get, setinv, getinv
    
    # Set the matrix x
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    # Get the matrix x
    get <- function() x
    # Set the inverse matrix of x
    setinv <- function(solve) m <<- solve
    # Get the inverse matrix of x
    getinv <- function() m
    
    # Return the list of accessor functions
    list(set = set,
         get = get,
         setinv = setinv,
         getinv = getinv)
}


cacheSolve <- function(accessors, ...) {
    # Return the inverse matrix
    # 
    # Args:
    #    accessors: a list of accessor functions
    # Returns:
    #    The inverse matrix of x
    
    # Get the inverse of x
    invx <- accessors$getinv()
    if(!is.null(invx)) {
        message("getting cached data")
        return(invx)
    }
    # Calculate the inverse o x
    x <- accessors$get()
    invx <- solve(x, ...)
    accessors$setinv(invx)
    
    invx
}

