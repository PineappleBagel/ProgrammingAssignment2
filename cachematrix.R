## This function will make a special matrix object that will cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL ## Initializes the inverse as NULL
  
  set <- function (y)
  x <<- y
  inv <<- NULL ## Resets the inverse if matrix is reset
}
get <- function()x

setinverse <- function(inverse) inv <<- inverse
getinverse <- function () inv

list(set = set,
     get = get,
     setinverse = setinverse,
     getinverse = getinverse)

## This function will compute the inverse of the special matrix from makeCacheMatrix
## If the matrix does not change when the inverse is already calculated then the cachesolve
## will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        
        if(!is.null(inv)) { 
          message("retrieving cached data")
          return(inv)
        }
        
        data <- x$get()
        inv <- solve (data, ...)
        x$setinverse(inv)
        inv
}
