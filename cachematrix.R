## These functions cache the inverse of a matrix

## This function creates a slist of functions to:
##    1. set the value of the matrix
##    2. get the value of the matrix
##    3. set the inverse of the matrix
##    4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set=set, get = get, setinverse = setinverse, 
             getinverse = getinverse)
}


## This function checks to see if the invrse matrix is cached.
## If so, it returns the inverse matrix.  If not, it calcuates
## the inverse matrix and caches the data.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
