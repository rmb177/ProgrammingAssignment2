## These functions provide a representation of a matrix
## that caches its inverse while its contents remain
## the same. If the matrix is update the cache is cleared
## and its inverse will be recalculated the next time
## cacheSolve is called

## Returns a list of methods that represent a matrix
## that caches its inverse
makeCacheMatrix <- function(data = matrix()) 
{
    inverse <- NULL
    set <- function(inData)
    {
        data <<- inData
        inverse <<- NULL
    }
    get <- function() data
    setInverse <- function(inInverse) inverse <<- inInverse
    getInverse <- function() inverse
    
    list(set = set,
     get = get,
     setInverse = setInverse,
     getInverse = getInverse)
}


## Return a matrix that is the inverse of 'x'. This function
## will return a cached value if it exists
cacheSolve <- function(x, ...) 
{
    inverse <- x$getInverse()
    if (!is.null(inverse))
    {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
    inverse
}
