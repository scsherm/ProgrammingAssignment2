## Functions to compute the inverse of a matrix unless it is is already computed, in which it will return the cached data.

## makeCacheMatrix creates a matirx which is really a list with function to:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        ## creates a function for set
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        ## creates a function for get
        get <- function() x
        ## creates a function for setinverse
        setinverse <- function(solve) inv <<- solve
        ## creates a function for getinverse
        getinverse <- function() inv
        ## creates a list of the functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve calculates the inverse of a matrix created with makeCacheMartix.
## It first checks to see that the inverse has not already been computed, in which it will return the cached inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        ## if statement to check if the inverse is already computed i.e. inv is NOT null.
        if(!is.null(inv)) {
                ## returns cahced data if the inverse is already computed.
                message("getting cached data")
                return(inv)
        }
        ## Calculates and returns the inverse using the solve function.
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
