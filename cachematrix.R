## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
     ## inverse is assigned with NULL, within the scope of this function
     inverse <- NULL
     
     setmatrix <- function(y = matrix()) {
          x <<- y
          inverse <<- NULL
     }
     getmatrix <- function() x
     setmatrix <- function(solve) inverse <<- solve
     getinverse <- function() inverse
     list(setmatrix = setmatrix, getmatrix = getmatrix,
          setmatrix = setmatrix,
          getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     inverse <- x$getinverse()
     if(!is.null(m)) {
          message("getting cached inverted matrix")
          return(inverse)
     }
     data <- x$getmatrix()
     inverse <- solve(data)
     x$setmatrix(inverse)
     inverse
     
}
