## This file is solution to Programming Assignment 2 in the coursera course 
## R Programming (https://class.coursera.org/rprog-008)
## It gives a solution for computing and storing the inverse of a matrix in a way
## that it caches the result and it is possible to use multiple times. 

## The function below creates a special matrix object

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


## The following function calculates the inverse of a matrix. It first checks if the inverse
## has already been calculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
