## These functions allow you to create a special object that holds a matrix and a cache of it's inverse.
## This allows for faster computations so that you don't have to keep calculating the inverse of a matrix.

## This function creates a special 'matrix' object that can cache it's inverse.
makeCacheMatrix <- function(x = matrix()) {
    invMatrix <- NULL ## where to store the inverse Matrix
    set <- function(y){
        x <<- y
        invMatrix <<- NULL  ## every time the matrix is changed, the cached inverse is deleted, so the wrong values aren't stored.
    }
    get <- function() x
    setInv <- function(solve) invMatrix <<- solve  ## computes the Inverse of the Matrix and stores it
    getInv <- function() invMatrix
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv) ## returns the 'matrix' object along with the Inverse 
}

## This function checks to see if there is a cache of the inverse (to save computation time)
## If not, then it computes the inverse itself
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invMatrix <- x$getInv()
    if(!is.null(invMatrix)){  ## Checks to see if there is a cached inverse of the Matrix
        message("Retrieving Cached Data: Matrix Inverse")
        return(invMatrix)
    }
    specialMatrix <- x$get()  ## If not, it's goes through computation itself and set the cached value to it.
    invMatrix <- solve(specialMatrix, ...)
    x$setInv(invMatrix)  ## Here it caches the inverse so that through future computations, it doesn't need to do it
    invMatrix
}