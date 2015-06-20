## Put comments here that give an overall description of what your
## functions do

## This first function takes an input, a matrix, calculates its inverse and store the values.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL                              ##inverse set to null
    set <- function(y) {                   ##matrix introduced into the function
        x <<- y
        m <<- NULL
    }
    get <- function() x                    ## 'get' returns the matrix stored before
    setsolve <- function(solve) m <<- solve ##calculates and stores the inverse
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

## This function calculates the inverse of a matrix. But first it tries to load the value, just in case it
##has been already calculated (and the matrix has not changed)

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()                   ## retrieves the value of the inverse of the matrix
    if(!is.null(m)) {                   ## check if that value exists and it's not null.
        message("getting cached data")
        return(m)
    }
    data <- x$get()                     ##calculates the inverse of a matrix if the above procedure
    m <- solve(data, ...)               ## didn't end up with an inverse of a matrix.
    x$setsolve(m)
    m
}