## makeCacheMatrix makes a list of functions to set/call a matrix and its inverse
## cacheSolve uses those functions to check for a cached inverse matrix, if not
## it calculates the inverse matrix 

## Creates a list containing functions to cache and call a matrix,
## and to catch and call its inverse 
makeCacheMatrix <- function(x = matrix()) {
    
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inverse <<- solve 
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Checks to see if the inverse of a matrix has been cached,
## if not it calculates the inverse matrix using solve(), caches it, and prints it

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
