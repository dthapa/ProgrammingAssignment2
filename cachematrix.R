## Inverting matrices is a computationally intensive task
## So unless the invertible square matrix has changed, 
## its inverse is retrieved from cache after the initial calculation

## Returns a CacheMatrix object with several nested functions
## for any matrix as an input
## in a list to get original matrix, save the calculated inverse and
## finally get the cached inverse (NULL if not set atleast once)
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    get <- function() x
    getinverse <- function() inv
    setinverse <- function(inverse) inv <<- inverse
    list(get = get, getinverse = getinverse, 
         setinverse = setinverse)
}

## Takes a CacheMatrix object and decides if there is a cached solution
## already available, if not, performs the computation and stores the
## result for future cache retrieval
## Inverted matrix for a valid square matrix is only calculated once
## and there after always retrieved from the cache
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message('Getting cached data')
        return(inv)
    }
    x$setinverse(solve(x$get(), ...))
    x$getinverse()
}
