## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    ## This function should create an inversion of a matrix and caches it.
    inverse_mat <- NULL
    set <- function(y) {
        x <<- y
        inverse_mat <<- NULL
    }
    get <- function() x
    set_inverse <- function(inverse) inverse_mat <<- inverse
    get_inverse <- function() inverse_mat
    list(set=set, get=get, set_inverse=set_inverse, get_inverse=get_inverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x', by finding in the get_inverse() or solve it.
    inverse_mat <- x$get_inverse()
    if(!is.null(inverse_mat)) {
        message("getting cached data")
        return(inverse_mat)
    }
    data <- x$get()
    inverse_mat <- solve(data, ...)
    x$set_inverse(inverse_mat)
    inverse_mat
}
