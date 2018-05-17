## Write a pair of functions, "makeCacheMatrix" and "cachemean" 
## that cache the inverse of a matrix
## makeCacheMatrix is a function which creates a special "matrix" object that can
## cache its inverse for the input (which is an invertible square matrix)
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
            x <<- y
            inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## cachemean is a function which computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachemean should retrieve the
## inverse from the cache

cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}
## ---------------Checking the program------------------------
## m <- matrix(rnorm(16),4,4)
## m1 <- makeCacheMatrix(m)
## cachemean(m1)

## [,1] [,2] [,3] [,4]
## [1,] -0.1653269 0.2592203 0.6176218 -0.7520955
## [2,] 0.2828334 -0.1853499 0.4511382 0.2094365
## [3,] 0.1434840 1.0413868 -0.3550853 -0.3261154
## [4,] 0.1793583 -0.4252171 -0.4371493 -0.1749830
