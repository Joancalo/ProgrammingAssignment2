## The function “makeCacheMatrix” should create a matrix that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        ## (1) set is a function that sets the value of the matrix.
        set <- function(y) {
                 x <<- y
                 m <<- NULL
        }
        
        ## (2) get is a function that returns the matrix stored in the main function.
        get <- function() x
        
        ## (3) setinverse and getinverse are functions that simply store the value of the input in a variable m
        ## (4) into the main function makeCacheMatrix (setinverse) and return it (getinverse).
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}   

## The function cacheSolve computes the inverse of the “matrix” returned by makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## (1) If the inverse has already been calculated, then the cacheSolve should retrieve the inverse from the cache.
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## (2) If the inverse has not been calculated, data gets the matrix stored with makeCacheMatrix, 
        ## (3) m calculates the inverse, and x$setinverse(m) stores it in the object m in makeCacheMatrix.
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
                
               
