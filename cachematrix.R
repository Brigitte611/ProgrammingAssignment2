## Calculation of the inverse of a matrix. 

## Function makeCacheMatrix creates a list with four functions.
## The first functions store (set) and return (get) the matrix in the cache.
## The other functions store (setinv) and return (getinv) the inverse 
## of the matrix (if available) in the cache.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, 
         get = get, 
         setinv = setinv, 
         getinv = getinv)
}


## Function cacheSolve looks up whether an inverse for the matrix is available in the cache:
## if yes, the inverse from the cache is used.
## if no, the inverse is calculated.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    message("new calculation")
    inv <- solve(data,...)
    x$setinv(inv)
    inv
}

