## This functions cache the inverse of a matrix



## makeCacheMatrix(x) function creates a special "matrix" object that can cache
## its inverse.

## Note: Assumption is that the matrix supplied is always invertible

makeCacheMatrix <- function(x = matrix()) {
        ## Return a list containing a function to:
        ## * set the value of the matrix
        ## * get the value of the matrix
        ## * set the value of the inverse (solve)
        ## * get the value of the inverse (solve)
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}



## cacheSolve(x, ...) function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix

## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("Getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
