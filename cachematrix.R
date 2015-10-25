##Inverse martrix calculations are often slow and computational costly.
##Inverse Matrices can be solved using the solve() function in R but the
##following two functions provide an alternative implementation to solve for
##inverse matrices.  The first creates an object to a store a matrix and its 
##inverse in a cache.  The second function then solves for the inverse of any 
##matrix but first checks to see if the same matrix is in the cache and 
##uses that result if it is.  

##The makeCacheMatrix function does 4 things.  It creates a special 
##matrix and enables you to set and retrive the matrix from it and to set 
##and retreive the inverse matrix from it.
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


##The following function uses the first function to solve for the inverse of a 
##matrix.  It before calling the solve function, it will check the cache to see
##if the the matrix has been solved before.  If it has, it will return the 
##value from the cache.
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
