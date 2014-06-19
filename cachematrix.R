## makeCacheMatrix creates a special "matrix", which is really a list
## containing functions to
## ["set"] set the value of the matrix
## ["get"] get the value of the matrix
## ["setinverse"] set the value of the matrix's inverse
## ["getinverse"] get the value of the matrix's inverse

makeCacheMatrix <- function(x = matrix()) {
    ## Set inv to null
    inv <- NULL
    
    ## Create 'set' function. This initiates x and sets inv to null
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    ## Create 'get' function. This simply retrieves x (from where it was 'set').
    get <- function() x
    
    ## Create 'setinverse' function. This takes a variable and sets inv.
    setinverse <- function(inverse) inv <<- inverse
    
    ## Create 'getinverse' function. This retrieves inv (from 'setinverse').
    getinverse <- function() inv
    
    ## Output the list of the above 4 functions.
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve calculates the inverse of the special "matrix" created with
## makeCacheMatrix.
## It first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets the value of
## the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## Call 'getinverse' to see if inv has already been set.
    inv <- x$getinverse()
    
    ## If inv isn't NULL, show the 'getting cached data' message and output inv.
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    ## If inv hasn't been calculated, get the matrix so we can invert it.
    data <- x$get()
    
    ## Solve the inverse of the matrix as inv.
    inv <- solve(data, ...)
    
    ## Set inverse of the CacheMatrix for future reference using 'setinverse'
    x$setinverse(inv)
    
    ## Output the inverse that had been calculated.
    inv
}
