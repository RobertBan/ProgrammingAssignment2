###############################################################################
## Introduction to R Programming                                             ##
## Programming Assignment 2: Lexical Scoping                                 ##
## Compute the inverse of a matrix and keep it in a cache in order           ##
## to avoid recomputing when the inverse of the same matrix is               ##
## needed. The matrix is presumed invertible.                                ##
###############################################################################

## Function: makeCacheMatrix
## Returns a list of functions:
## - set:               stores the matrix to be inverted
## - get:               returns the matrix to be inverted
## - setInverse:        stores the inverse of the matrix
## - getInverse:        returns the inverse of the matrix
##
## Usage:
## Create list of functions:
## > mCacheObj <- makeCacheMatrix(MatrixToBeInverted)
## Set or change the matrix to be inverted 
## > mCacheObj$set(MatrixToBeInverted)
## Retrieve the matrix to be inverted
## > mToInvert <- mCacheObj$get()
## Store the inverse:
## > mCacheObj$setInverse(mInverted)
## Retrieve the inverse:
##> mInverted <- mCacheObj$getInverse()

makeCacheMatrix <- function(x = matrix()) {
        mInverse <- matrix(data = NA)
        
        ## Stores the matrix sent via argument y and resets the cached inverse
        ## (only in case y is different than the already stored matrix)
        set <- function(y=matrix()) {
                if (identical(x, y)) return()
                x <<- y
                mInverse <<- as.matrix(NA)
        }
        
        ## Retrieves the matrix to be inverted
        get <- function() {
                x
        }
        
        ## Stores the inverse matrix in cache
        setInverse <- function(Inverse) {
                mInverse <<- Inverse
        }
        
        ## Retrieves the inverse matrix from the cache
        getInverse <- function() {
                mInverse
        }
        
        ## Create and return the list of functions
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Function: cacheSolve
## Calls the functions stored in a list created with makeCacheMatrix
## in order to retrieve the cached inverse of a matrix (or compute
## and cache it, if not available).
##
## Usage:
## > mCacheObj <- makeCacheMatrix(MatrixToBeInverted)
## > mInverseMatrix <- cacheSolve(mCacheObj)

cacheSolve <- function(x, ...) {
        ## Retrieve the inverse matrix from the cache
        mInverse <- x$getInverse()
        if(!any(is.na(mInverse))) {
                message("getting cached data")
                return(mInverse)
        }
        
        ## If the inverse matrix was not available, retrieve
        ## the matrix, compute and cache its inverse, and the
        ## return the inverse
        data <- x$get()
        mInverse <- solve(data, ...)
        x$setInverse(mInverse)
        mInverse
}
