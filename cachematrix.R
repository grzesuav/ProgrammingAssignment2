## Put comments here that give an overall description of what your
## functions do

## wrapper for original matrix and its inversion

makeCacheMatrix <- function(originalMatrix = matrix()) {
    cachedInverse <- NULL #initialize inverse to null
    set <- function(y) { # setter for original matrix, also nullify cached inverse
        originalMatrix <<- y
        cachedInverse <<- NULL
    }
    get <- function() originalMatrix
    setInverse <- function(inverse) cachedInverse <<- inverse # setter for inverted matrix
    getInverse <- function() cachedInverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## solver for wrapped matrices, calculates or return cached value of inverted matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse() # get inverted matrix from cache
    if(!is.null(m)) {  # if not null -> return cached inversion
        message("getting cached data")
        return(m)
    }
    # if null -> calculate inversion
    matrix <- x$get() 
    inversion <- solve(matrix) # calculate inversion of "matrix"
    x$setInverse(inversion) # cache calculated value
    inversion # return inverted matrix
}
