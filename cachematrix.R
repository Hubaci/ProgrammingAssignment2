## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        invm <- NULL
        set <- function(y) {
                x <<- y
                invm <<- NULL
        }
        get <- function() x
        setinverm <- function(inverm) invm <<- inverm
        getinverm <- function() invm
        list(set = set, get = get,
             setinverm = setinverm,
             getinverm = getinverm)

}


## The function cacheSolve computes the inverse of the special "matrix" 
## returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invm <- x$getinverm()
        if(!is.null(invm)) {
                message("getting cached data")
                return(invm)
        }
        data <- x$get()
        invm <- solve(data, ...)
        x$setinverm(invm)
        invm  
        
}
