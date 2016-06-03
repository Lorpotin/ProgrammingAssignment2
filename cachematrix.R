

## function to create a special matrix which inverse can be cached
makeCacheMatrix <- function(x = matrix()) {
    
    ## initialize the inversion variable
    m <- NULL
    
    ## set the matrix
    set <- function(y) {
        ## <<- assigns a value in an environment different from the current one
        x <<- y
        m <<- NULL
    }
    
    ## get the matrix
    get <- function() {
        ## return the matrix
        x
    }
    
    ## set the value of the inverse
    setInverse <- function(inverse) {
        ## set the inversion variable as the given parameter
        m <<- inverse
    }
    
    ## return the inverse value, m
    getInverse <- function() {
        m
    }
    
    ## return a list of the earlier methods, since we have functions inside a function
    ## otherwise it wouldn't be possible to access these
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## function that computes the inverse of the "matrix" (list returned in the earlier function)
## if the inverse has already been calculated and the matrix is the same, then the cacheSolve should retrieve from cache.
cacheSolve <- function(x, ...) {
    
    ## inverse a matrix 'x'
    m <- x$getInverse()
    
    ## if the inverse is already set, return it and skip the rest of the function
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ## if the earlier if clause 
    ## get the matrix 
    data <- x$get()
    
    ## calculate the inverse using matrix multiplication
    m <- solve(data, ...)
    
    ## set the value of the inverse in the cache
    x$setInverse(m)
    
    ## return the inversed matrix
    m
    
}
