## Put comments here that give an overall description of what your
## functions do
#
# The function makeCacheMatrix creates a list of functions
# that manipulate the variables inside makeCacheMatrix's
# environment, so that it is only possible to alter
# the matrix 'x' and its inverse by using the setter
# and getter functions. This way, it is possible to store
# the inverse of 'x' for future uses without having to
# compute it all repeatedly.
#
# The function cacheSolve manages the values stored in
# the makeCacheMatrix enviroment in order to ensure that
# the values of the matrix 'x' and its inverse are always
# sychronised. In case they are not, cacheSolve proceeds
# to the updating of the inverse matrix stored in
# makeCacheMatrix's environment


## Write a short comment describing this function
# makeCacheMatrix
# accepts: numeric matrix 'x'
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
        x <<- mat
        xinv <<- NULL
    }

    get <- function() x

    setinv <- function(matinv) xinv <<- matinv

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
# the inverse stored in 'cached_inv' correponds to the inverse
# of the matrix in the specified matrix object 'x' by following
# the logic:
#
# If cached_inv != NULL, then the inverse of the matrix in 'x'
#     has already been computed and 'cached_inv' has already
#     been updated.
# Else, calculate the inverse of the matrix in 'x', update
#         'cached_inv', assuming it is invertible.
# Return the restored/calculated 'cached_inv'
#
# Note that if the matrix in 'x' is empty, then a 1-by-1
# matrix with a NA entry will be returned.
#
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    cached_inv <- x$getinv()
    if(!is.null(cached_inv)) {
        message("Returning cached data.")
    }
    else {
        mat <- x$get()
        cached_inv <- solve(mat, ...)
        x$setinv(cached_inv)
    }
    cached_inv
}
