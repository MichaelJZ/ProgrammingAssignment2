##makeCacheMatrix saves a matrix or it's inverse, and can display them
##cacheSolve determines the inverse of a matrix and caches it, if there isn't an inverse already cached

## makeCacheMatrix creates a special "matrix", which is a list containing a function to
## save the matrix with the set function
## display the matrix with the get function
## save the inverse of the matrix with the setinverse function
## display the inverse of the matrix with the getinverse function

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve checks if the inverse to a matrix is already cached and displays it if it already exists
## if not, cacheSolve with calculate the inverse of the matrix and cache it

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i<- solve(data, ...)
    x$setinverse(i)
    i
}
