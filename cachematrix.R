## the two functions below create a special object that stores a
## numeric matrix and caches its inverse

## the makeVector function creates a special list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix 
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## the cacheSolve function calculates the inverse of the special "matrix" of makeCacheMatrix
## it first checks to see if the inverse has been calculated already
## if so, cacheSolve gets this value and skips the rest of the computation
## if not, cacheSolve calculates the inverse of the data and sets that value in the cache via the setinverse function


cacheSolve <- function(x, ...) {
    m <- m$getinverse() 
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
