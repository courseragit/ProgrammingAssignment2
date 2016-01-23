# In order to test these functions, exeute the following steps after compiling each:
# Create a seed matrix
# > x <- matrix(c(1, 2, 3, 4), nrow=2, ncol=2) 
# Use makeCacheMatrix to create a new matrix
# > y <- makeCacheMatrix(x) 
# Execut cacheSolve to return the inverse of the above matrix and place in cache.
# > cacheSolve(y)  
# Execute again. Upon second and subsequent executions, you should see the message "returning cached information".
# > cacheSolve(y)                           
# > cacheSolve(y)                                             

# makeCacheMatrix
makeCacheMatrix <- function(x = matrix()) {
    # inv will store the cached inverse matrix
    invrs <- NULL

    set <- function(y) {
        x <<- y
        invrs <<- NULL
    }
    get <- function() x
    setinvrs <- function(inverse) invrs <<- inverse
    getinvrs <- function() invrs
    
    # Return
    list(set = set, 
         get = get, 
         setinvrs = setinvrs, 
         getinvrs = getinvrs)
}


# This function computes the inverse of the matrix. If the inverse has already
# been calculated, the function returns the cached value rather than re-calculating.
cacheSolve <- function(x, ...) {
    invrs <- x$getinvrs()
    
    # Test to see if we have already cached
    if (!is.null(invrs)) {
        message("returning cached information")
        return(invrs)
    }
    data <- x$get()
    invrs <- solve(data, ...)
    

    x$setinvrs(invrs)
    
    # Return the result
    invrs
}
