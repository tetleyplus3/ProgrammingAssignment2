## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
     ## 
     inverse <- NULL
     
     setmatrix <- function(y = matrix()) {
          x <<- y
          inverse <<- NULL
     }
     getmatrix <- function() x
     setmean <- function(mean) m <<- mean
     getinverse <- function() inverse
     list(setmatrix = setmatrix, getmatrix = getmatrix,
          setmean = setmean,
          getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     m <- x$getmean()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- mean(data, ...)
     x$setmean(m)
     m
     
}
