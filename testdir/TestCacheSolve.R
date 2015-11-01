test.CacheSolveInversion <- function() {
    A <- matrix(c(2, -1, 0, -1, 2, -1, 0, -1, 2), c(3, 3))
    matobj <- makeCacheMatrix(A)
    # Calculate the inversion using the function from R library
    Ainv <- solve(A)
    # Use the function cache solve
    cacheAinv <- cacheSolve(matobj)
    # Compare results
    checkEqualsNumeric(Ainv, cacheAinv)
    checkEqualsNumeric(Ainv, matobj$getinv())
}

test.CacheSolveNull <- function() {
    # If we try to compute the inverse of NULL, NA should be returned.
    matobj <- makeCacheMatrix()
    checkEquals(matrix(), matobj$get())
    checkEquals(NULL, matobj$getinv())
    resNA <- cacheSolve(matobj)
    checkEquals(matrix(as.numeric(NA)), resNA)
    checkEquals(matrix(as.numeric(NA)), matobj$getinv())
}
