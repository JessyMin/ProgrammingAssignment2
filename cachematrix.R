# Following functions are used to cache the inverse of a matrix.


# makeCacheMatrix creates a matrix

makeCacheMatrix <- function(x = matrix()) {
        inv_m <- NULL
        set <- function(y) {
                x <<- y
                inv_m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv_m <<- inverse
        getinverse <- function() inv_m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)        
}


# cacheSolve makes inverse of a matrix. 

cacheSolve <- function(x, ...){
        inv_m <- x$getinverse()
        if(!is.null(inv_m)) {
                message("getting cached data")
                return(inv_m)
        }
        data <- x$get()
        inv_m <- solve(data)
        x$setinverse(inv_m)
        inv_m
}
