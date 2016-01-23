# In order to test these functions, exeute the following steps after compiling each:
# Create a seed matrix
# > x <- matrix(rnorm(18), nrow = 6) 
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
    inv <- NULL

    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    
    # Return
    list(set = set, 
         get = get, 
         setinv = setinv, 
         getinv = getinv)
}


# This function computes the inverse of the matrix. If the inverse has already
# been calculated, the function returns the cached value rather than re-calculating.
cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    
    # Test to see if we have already cached
    if (!is.null(inv)) {
        message("returning cached information")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    
    x$setinv(inv)

    inv
}
