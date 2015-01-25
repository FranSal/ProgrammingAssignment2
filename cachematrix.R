
## MakeCasheMatrix takes a matrix object and makes a mirror copy of it 
## that allow to store its value and inverse matrix.
##  set  first checks if the X argument is DIFFERENT ( !identical()) to a potentially stored value. 
## if it's diferent it caches the new value on X and sets the iM( inverse matrix) to NULL if it's identical
## then it does nothing. This is the function to use to modify the matrix, once the cached object has been created
## by makeCacheMatrix(X)

## iM is the inverse matrix cache
## X is the cache to X argument
## get delivers X value back from the  X cache
## setinv stores the inverse matrix ( that is its argument X) to the iM cache
## getinv takes the cached value and returns it.
## MakeCacheMatrix is the maker of a cached matrix object. To modify the matrix object itself
## X$set on the cached object  must used. this function checks for equiality. Using the naked 
## makeCacheMatrix will reset the cached object to another object, erasing its cache.

makeCacheMatrix <- function(X = matrix()) {
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

## cachesSolve  takes the X argument and returns its inverse matrix.
## it first checks if the inverse matriz (iM) existe.
## if it does exist, then it recovers its value from cache
## if it's NULL then it calculates its value and stores it in its cache 
## using setinv on its argument.

cacheSolve <- function(X, ...) {
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
