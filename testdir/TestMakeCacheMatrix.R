# test. the makecache matrix ability to recover the original matrix
test.MakeCacheMatrixGet <- function() {
    # Store matrix
    A <- matrix(1:6, 2, 3)
    matobj <- makeCacheMatrix(A)
    # Compare the stored matrix with the original one to test the get function
    checkEquals(dim(matobj$get()), c(2, 3))
    checkEquals(matobj$get(), A)
}

# test. the makecache matrix ability to change the original matrix
test.MakeCacheMatrixSet <- function() {
    # Store matrix
    A <- matrix(1:6, 2, 3)
    matobj <- makeCacheMatrix(A)
    # Change matrix using the set function
    B = matrix(c(9, 8, 7, 6, 5, 4, 3, 2), c(4, 2))
    matobj$set(B)
    # Compare the stored matrix with the original one to test the get function
    checkEquals(dim(matobj$get()), c(4, 2))
    checkEquals(matobj$get(), B)
}

# test. the ability to store and retrieve the inverse of a matrix
test.MakeCacheMatrixGetinv <- function() {
    # Store a matrix as a inverse of some other matrix
    A <- matrix(c(3, 7, 2, 6, 4, 5, 1, 9, 8), c(3, 3))
    Ainv <- solve(A)
    matobj <- makeCacheMatrix(A)
    matobj$setinv(Ainv)
    # Compare the results
    checkEqualsNumeric(matobj$getinv(), Ainv)
    # See if the returned matrix is both a left and a right inverse
    ident <- matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 1), c(3, 3))
    checkEqualsNumeric(matobj$getinv() %*% A, ident)
    checkEqualsNumeric(A %*% matobj$getinv(), ident)
}

# test. if the inverse matrix is removed when the original matrix is updated
test.MakeCacheMatrixSetinv <- function() {
    # Store a matrix as a inverse of some other matrix
    A <- matrix(c(3, 7, 2, 6, 4, 5, 1, 9, 8), c(3, 3))
    Ainv <- solve(A)
    matobj <- makeCacheMatrix(A)
    matobj$setinv(Ainv)
    # Compare the results
    checkEqualsNumeric(matobj$getinv(), Ainv)
    # See if the returned matrix is both a left and a right inverse
    ident <- matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 1), c(3, 3))
    checkEqualsNumeric(matobj$getinv() %*% A, ident)
    checkEqualsNumeric(A %*% matobj$getinv(), ident)
    # Update the original matrix
    B <- matrix(c(2, -1, 0, -1, 2, -1, 0, -1, 2), c(3, 3))
    matobj$set(B)
    # The result returned by getinv should be a NULL
    checkEquals(matobj$getinv(), NULL)
}
