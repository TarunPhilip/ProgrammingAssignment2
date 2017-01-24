## Put comments here that give an overall description of what your
## functions do

## Cache of the Inverse of a Matrix:
## I have 2 functions to stores a matrix and cache it's inverse

## Function 1 - makeCacheMatrix creates a list containing a function to
	# 1. set the value of the matrix
	# 2. get the value of the matrix
	# 3. set the value of inverse of the matrix
	# 4. get the value of inverse of the matrix


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

## Function 2 - To return a matrix that is the inverse of 'x'

## The next function will compute the inverse of the matrix created by makeCacheMatrix. While calculating the inverse, if the matrix does not change, it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
