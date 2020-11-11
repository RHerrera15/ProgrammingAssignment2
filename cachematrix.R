## This functions cache the inverse of a matrix 
## in other words, store the value of the inverse matrix
## and if the user ask again for the same inverse do not calculate it
## again just gives the value that was stored.

## This function just initialize values and some functions to
## help to the cache

makeCacheMatrix <- function(x = matrix()) {
    
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(solve) i <<- solve
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function gives back the inverse of the matrix if it's the first time
## and if it is the same matrix gives back the cache

cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinv() ## We ask for the value in i 
    if(!is.null(i)) { 
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
    
}
