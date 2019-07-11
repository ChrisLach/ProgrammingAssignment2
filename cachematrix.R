## Computing the inverse of a square matrix can be done with the solve function in R. 
## For example, if X is a square invertible matrix, then solve(X) returns its inverse.

## Computation of such Inverse often involve time-consuming computations.

## If the contents of a matrix are not changing, it will make sense to cache the value of the Inverse
## so that when we need it again, it can be looked up in the cache rather than recomputed.

## Aimed at this, I have created two functions, a first one called makeCacheMatrix 
## and a second one named cacheSolve.  Brief description follows >>>

## 1) makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = numeric()) {
    mat = NULL
    set <- function(y) {
        x <<- y
        mat <<- NULL
    }
    get <- function() x
    setInverseMat <- function(solve) x <<- InverseMat
    getInverseMat <- function() mat
    list(set = set, get = get, setInverseMat = setInverseMat, getInverseMat = getInverseMat)
}


## 2) cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    m <- x$getInverseMat
    if(!is.null(mat)) {
        message("Getting Cached Data")
        return(mat)
    }
    matrix <- x$get()
    m <- solve(matrix, ...)
    x$setInverseMat(mat)
    mat
        ## Return a matrix that is the inverse of 'x'
}
