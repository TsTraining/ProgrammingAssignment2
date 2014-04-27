## Two functions to calculate and cache the result of a matrix inversion

## The following function creates a list of functions that provide setting (set) and
getting (get) a matrix and setting (setinv) and getting (getinv) the inversion of that matrix

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function takes the returned value of the first function and either calculates
the inversion of the matrix and caches the result, or - if available - it retrieves the
cached result of a previous inversion calculation for that matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv

}
