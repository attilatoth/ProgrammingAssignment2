## The two functions below create matrix inverses that are being 
## cached the very first time, and after that the cached result is returned
##  (without  any computation).


## Returns a list containing functions that
## gets/sets the value of the matrix, and
## gets/sets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(other) {
        x <<- other
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inverse <<- solve
    getinverse <- function() inverse
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## Calculates the inverse of the special "matrix" created with the above function.
## Returns cached result if inverse has already been created.

cachesolve <- function(x, ...) {
    inverse <- x$getinverse()
    if (!is.null(inverse)) {
        message("getting cached data")
    return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
