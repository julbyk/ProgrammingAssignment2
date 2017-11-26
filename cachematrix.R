## Functions allow to cache the value of the matrix inverse.
## If the matrix haven't been changed, the inverse of the matrix is not being computed again,
## but it is looked up in a cache.

## Function makeCacheMatrix sets the environment which stores the matrix (x) and it's inverse (inv).
## It also defines four functions: set() and get() to set and retreive the matrix value, respectively,
## setinverse() and getinverse() to set and retrieve the matrix inverse, respectively.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
## If the function set() is called, the matrix function is updated and inverse of the matrix becomes NULL.
## That updates previously cached value.
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
## After new inverse of the matrix is calculated, the function setinverse() updates the value for inverse.
    setinverse <- function(minv) inv <<- minv
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The function cacheSolve() returns the matrix inverse.
## If the matrix inverse is already cached, the function returns the message 'getting cached data'
## and the cached matrix inverse.
## If the matrix inverse is not cached previously, it is calculated, returned, and cached.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
## Retrieve the value of the matrix inverse from the cache.
    inv <- x$getinverse()
## If matrix inverse is not NULL, the inverse fot the cached matrix is already calculated
## and is retrieved from cache. After this the function is exited.
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
## If matrix inverse is NULL, there is no cached matrix inverse stored.
## The matrix inverse is calculated from the data using solve() function, returned,
## and cached by using the function setinverse().
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
