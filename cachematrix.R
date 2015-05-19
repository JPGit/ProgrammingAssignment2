##
## Solve the inverse of a invertible matrix, 
##  caching the value after the first calculation
##
## Example calls:
##   x <- makeCacheMatrix(matrix(1:4, 2, 2))
##   cacheSolve(x)

## makeCacheMatrix 
## Create a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    
    ## initialize value
    m <- NULL
    
    ## set value function
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    ## get value function
    get <- function() { x }
    
    ## set inverse value
    setsolve <- function(inverse) {
        m <<-inverse
    }
    ## get inverse value
    getsolve <- function() { m }
    
    ## return list of get/set functions
    list (set = set, 
          get =get,
          setsolve = setsolve,
          getsolve = getsolve)
    
    
}


## cacheSolve
## computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has been calculated and the matrix has not changed
## then retrieve the solution from cache

cacheSolve <- function(x, ...) {
    
    m <- x$getsolve()
    
    if (is.null(m)) {
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
    }
    else {
        message ("getting cached data")
    }
    
    m
}
