## Below are two functions that are used to create a special object
## that stores a square matrix and caches its inverse.


## The first function, makeCacheMatrix creates a special "vector",
## which is really a list containing a function to

##   set the value of the matrix
##   get the value of the matrix
##   set the value of the inverse
##   get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(ginv) inv <<- ginv
    getinv <- function() inv
    list(set = set, get = get,
    setinv = setinv,
    getinv = getinv)
}


## The following function calculates the inverse of the matrix.
## It first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets the value
## of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- m$getinv()
        if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
        matr <- x$get()
        inv <- solve(matr, ...)
        x$setinv(inv)
        inv
}
