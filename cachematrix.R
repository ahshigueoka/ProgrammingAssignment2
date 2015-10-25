## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makeCacheMatrix
# accepts: numeric matrix x
# returns: list of functions [set, get, setinv, getinv]
#
# This functions abstracts the creation of a matrix object that
# caches the value of its correspondent inverse in order to
# optimise code. It is assumed that an inversible matrix 'x' is
# given as an argument and returns a list of setter and getter
# functions.
#
makeCacheMatrix <- function(x = matrix()) {
    xinv <- NULL

    set <- function(mat) {
        x <- mat
        xinv <- NULL
    }

    get <- function() x

    setinv <- function(matinv) xinv <- matinv

    getinv <- function() xinv

    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
# cacheSolve
# accepts: matrix object returned by makeCacheMatrix
# returns: the inverse of the matrix stored in the matrix
#     object
#
# This function checks the variable cached_inv to see whether
# the inverse stored in cached_inv correponds to the inverse
# of the matrix in the specified matrix object 'x' by following
# the logic:
#
# If cached_inv != NULL, then the inverse of the matrix in 'x'
#     has already been computed and 'cached_inv' has already
#     been updated. So just return 'cached_inv'.
# Else, calculate the inverse of the matrix in 'x', update
#     'cached_inv' and return it.
#
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
}
