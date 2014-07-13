## A pair of functions that cache the inverse of a matrix

## Creates a special 'matrix', which is really a list containing a function to:
## set the matrix, get the matrix, set the inverse matrix, get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    imat <- NULL
    set <- function(y) {
        x <<- y
        imat <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) imat <<- inverse
    getinverse <- function() imat
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Calculates the mean of the special 'matrix' created with the above function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    imat <- x$getinverse()
    if (!is.null(imat)) {
        message("getting cached data")
        return(imat)
    }
    mat <- x$get()
    imat <- solve(mat)
    x$setinverse(imat)
    imat
} 
