## Matrix inversion is usually a costly computation and 
## there may be some benefit to caching the inverse of a matrix rather 
## than compute it repeatedly

## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to:
##   set the value of the matrix
##   get the value of the matrix
##   set the value of the inverse of the matrix
##   get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) s <<- solve
        getinverse <- function() s
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## The following function calculates the inverse of the special "matrix" 
##  created with the function makeCacheMatrix. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value in the 
##  cache via the setinverse function.

cacheSolve <- function(x, ...) {
		## Return a matrix that is the inverse of 'x'
        s <- x$getinverse()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setinverse(s)
        s
}
