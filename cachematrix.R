##Inverse martrix calculations are often slow and computational costly.
##Inverse Matrices can be solved using the solve() function in R but the
##following two functions provide an alternative implementation to solve for
##inverse matrices.  The first creates an object to a store a matrix and to 
##cache its inverse.  The second function then solves for the inverse matrix 
##but first checks to see if the same matrix is in the cache and uses that 
##result if it is.  

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    im <- NULL
    set <- function(y) {
        x <<- y
        im <<- NULL
    }
    get <- function() x
    setInverseMatrix <- function(answer) im <<- answer
    getInverseMatrix <- function() im
    list(set = set, get = get,
         setInverseMatrix = setInverseMatrix,
         getInverseMatrix = getInverseMatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    im <- x$getInverseMatrix()
    if(!is.null(im)) {
        message("getting cached data")
        return(im)
    }
    data <- x$get()
    im <- solve(data, ...)
    x$setInverseMatrix(im)
    im
}
