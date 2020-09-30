## This function mimics the similar functionality of caching

## Basically one function creates a special list which contains some coverage
## These coverages are indexed with <<- operator so that the values of parent environment
## can be changed

makeCacheMatrix <- function(x = matrix()) {
    parent.inverse <- NULL
    set <- function(y) {
        x <<- y
        parent.inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(child.inverse) parent.inverse <<- child.inverse
    getinverse <- function() parent.inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## Checks if inverse is present or not. If inverse is present, then does not compute it
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
    
}
