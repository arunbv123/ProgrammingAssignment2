## Put comments here that give an overall description of what your
## functions do

## This function creats a R object that stores the matrix and its inverse. 
## This function also has get and set functions to access both matrix 
## and matrix inverse externally

makeCacheMatrix <- function(x = matrix()) {
        matInv <- NULL
        set <- function(y) {
                x <<- y
                matInv <<- NULL
        }
        get <- function() x
        setInv <- function(mInv) matInv <<- mInv
        getInv <- function() matInv
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
        
}


## This function creates function that returns the matrix inverse, 
##      1) If the object already has inverse in cache
##         then this function feteches from the cache.
##      2) If the object doesnt have inverse then it computes 
##         and stores in cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mInv <- x$getInv()
        if(!is.null(mInv)) {
                message("getting cached data")
                return(mInv)
        }
        data <- x$get()
        mInv <- solve(data, ...)
        x$setInv(mInv)
        mInv
}
