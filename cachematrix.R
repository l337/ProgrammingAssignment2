## makeCacheMatrix takes as input a matrix
## first declares an m variable for storage / initialization
## set lets us override a matrix passed into makeCacheMatrix
## get lets us just retrieve the value of the matrix
## getinverse and setinverse lets us retrive and set the solved values for the matrix
## returns an accessable list to the methods if called outside the makeCacheMatrix function
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve takes a matrix created by makeCacheMatrix
## if the inverse of the matrix hasn't been solved (in other words isn't NULL)
## the input will *update* the inverse and store the value of m via superassignment
## otherwise if it is not null cacheSolve will just return the inverse of the stored value
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}