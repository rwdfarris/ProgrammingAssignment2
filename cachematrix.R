## makeCacheMatrix & cacheSolve are two functions that work together to define a matrix and calculate, cache
## and return its inverse.



## makeCacheMatrix utilizes either a defined or a default empty matrix within the environment of 
## the function.  It also clears the cache if the defined matrix is different than the previously defined 
## matrix and also defines a character named list of the individual objects within the environment of the 
## function that can subsequently be called from the 'cacheSolve' function in order to calculate the 
## inverse of either a new matrix or report the result of the inverse of the matrix that was previously
## calculated.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set=set, get=get,
             setinv=setinv,
             getinv=getinv)
}


## cacheSolve utilizes the objects cached and named in makeCacheMatrix above to calculate and return either a 
## newly entered and calculated matrix inverse or retrieve and return from the 'cache' the inverse of the 
## matrix that was previously returned if it is called repeatedly rather than calculating it again

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}