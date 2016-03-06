## The first fucntion (makeCacheMatrix) creates the list that the second function uses to calculate the inverse. 
## The second function (cacheSolve) then creates the invesrse of the matrix and saves it to the cache,as well as checking the cache.

## This function creates a list of a data matrix that sets the values of the matrix as well as the inverse of each datum in the matrix.

makeCacheMatrix <- function(x = matrix()) {
   v <- NULL
   set <- function(y) {
     x <<- y
     v <<- NULL
   }
   get <- function() x
   setinverse <- function(inverse) v <<- inverse
   getinverse <- function() v
   list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}

## This fucntion calculates the inverse of the matrix form the list created with the makeCacheMatrix fucntion, but it first checks
## to see if the values are already in the cache and if they are it retruns the data from the cache instead of recalculating the values.

cacheSolve <- function(x, ...) {
  v <- x$getinverse()
  if(!is.null(v)) {
    message("getting cached data")
    return(v)
  }
  data <- x$get()
  v <- solve(data, ...)
  x$setinverse(v)
  v
}

