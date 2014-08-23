###########################R Programming-Project 2 Caching the Inverse of a Matrix
########################### Jeff Roberts/23 August 2014


###makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
###cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix ####has not changed), then the cachesolve should retrieve the inverse from the cache


###This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmatrix <- function(matrix) m <<- matrix
    getmatrix <- function() m
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
}


####This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
    m <- x$getmatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(m) %*% m(data, ...)  ###Invert Matrix
    x$setmatrix(m)
    m
}