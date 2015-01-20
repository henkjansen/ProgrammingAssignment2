## Create a matrix object that can cache its inverse.
## set: set the matrix
## get: get the matrix
## setInverse: set the matrix inverse
## getInverse: get the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL

    set <- function(y) {
        x <<- y
        i <<- NULL
    }

    get <- function() {
        x
    }

    setInverse <- function(inverse) {
        i <<- inverse
    }

    getInverse <- function() {
        i
    }

    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Compute the inverse of a matrix.
## Get the result from the cache if possible.

cacheSolve <- function(x, ...) {
    i <- x$getInverse()

    if (! is.null(i)) {
        message("getting cached data")
        return (i)
    }

    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)

    i
}

