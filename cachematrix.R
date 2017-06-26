## Contains two functions: makeCacheMatrix, which inverts a matrix and caches the result,
##                                          outside of loops.
##                         cacheSolve,      which inside loops looks to see if the inverse
##                                          matrix is cached and if not calculates it.


##  makeVector creates a list of functions that
##  1. set the matrix
##  2. get the matrix
##  3. set the inverse matrix
##  4. get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL             # inv is the inverse matrix
        set <- function(y) {
            x   <<- y           # y is the initial matrix, x is it in cache
            inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) {inv <<- solve(y)}
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve checks to see if the inverse matrix mean has been calculated and cached,
## and if so it gets it from there. Otherwise, it calculates the inverse matrix.

cacheSolve <- function(x, ...) {
        ## x, here, is the output vector from makeCacheMatrix

        inv <- x$getinv()
        if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        ## x$setinv(m)  #instructions don't say to set cache as in cachemean
        inv
}
