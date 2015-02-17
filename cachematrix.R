## R Programming - programming assignment 2
## 
## Functions demonstrating use of an R object to cache the result of a
## potentially time-consuming operation

## Cacheing object used only in calls to cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
        ##
        ## Object used to cache the inverse of a square matrix
        ##
        ## Access this only via one of the
        ## list of named member functions
        ##
        ## x is the passed matrix
        ## 
        ## m is an internal variable that is the inverse of
        ## x if x is non-null and m is non-null (indicating
        ## that the inverse of the matrix x has already been
        ## requested). m is only calculated once, at the time
        ## its value is first requested by the getInverse
        ## function.
        
        m <- NULL
        setMatrix <- function(y) {
                ## replace the original matrix with another.
                if ( class(y) != 'matrix' ) {
                        stop("passed argument must be a matrix!")
                }
                x <<- y
                m <<- NULL
        }
        
        getMatrix <- function() {
                ## return the matrix either set during creation of this
                ## instance of makeCacheMatrix(), or by a subsequent
                ## call to setMatrix().
                return (x)
        } 
        
        setInverse <- function(inverse) {
                ## set the inverse of the matrix.
                if ( class(inverse) != 'matrix' ) {
                        stop("passed argument must be a matrix!")
                }
                m <<- inverse
        }
        
        getInverse <- function() {
                ## return current value of cached inverse (may be NULL 
                ## if setInverse() has not been called yet)
                return(m)
        }
        
        ## expose accessor functions
        
        list(setMatrix = setMatrix, 
             getMatrix = getMatrix,
             setInverse = setInverse,
             getInverse = getInverse)
}

## Return the inverse of a matrix cached in a makeCacheMatrix() object

cacheSolve <- function(x, ...) {
        ## Return the inverse of a matrix cached in an object created by calling
        ## makeCacheMatrix() passing the matrix to be inverted. If called N
        ## times without resetting the matrix, all calls after the first will
        ## return the cached value, and the message 'getting cached data' will 
        ## be displayed.
        
        ## x: object created by call to makeCacheMatrix()
        ##
        ## ...: additional arguments, if any, to be passed to solve().
        ##      These only take effect in the first call to cacheSolve()
        ##      since solve() is not called in subsequent invocations.
        
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$getMatrix()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
