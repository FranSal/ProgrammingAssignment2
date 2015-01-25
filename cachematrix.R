## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    iM <- NULL
    set <- function(Y) {
        if(!identical(Y,X)){
            X <<- Y
            iM <<- NULL
        }
    }
    get <- function() X
    setinv <- function(X) iM <<- X
    getinv <- function() iM
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    iM  <- X$getinv()
    if(!is.null(iM)) {
        message("getting cached data")
        return(iM)
    }
    data <- X$get()
    iM <- solve(data, ...)
    X$setinv(iM)
    iM
}
