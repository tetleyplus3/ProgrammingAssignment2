## Based on the makevector function in the example code for this assignment
## makeCacheMatrix is a list contains 4 functions
## 1 set the value of the matrix
## 2 get the value of the matrix
## 3 set the value of the inverse
## 4 get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
     ## inverse is assigned with NULL, within the scope of this function
     inverse <- NULL
     
     ## setmatrix set the value of the matrix
     setmatrix <- function(y = matrix()) {
          x <<- y
          inverse <<- NULL
     }
     getmatrix <- function() x ## Retrieves the matrix
     setinverse <- function(solve) inverse <<- solve
     getinverse <- function() inverse
     list(setmatrix = setmatrix, getmatrix = getmatrix,
          setmatrix = setmatrix,
          getinverse = getinverse)

}


## The following function verifies if the inverse of a matrix already exists
## if so, if the matrix did not change, then it retrieves the value of its
## inverse, else it calculates the inverse of the matrix.

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     
     ## First, we get the value of the inverse (it might also be NULL)
     inverse <- x$getinverse()
     ## Then, we verify if the inverse had been already calculated (not NULL)
     ## AND the matrix we want to calculate the inverse has not changed
     ## in which case, we simply return the value of the inverse
     if(!is.null(inverse) & (x == getmatrix()) ){
          message("getting cached inverted matrix")
          return(inverse)
     }
     
     ## If the matrix has changed OR the inverse was not previously calculated
     ## we proceed to calculate the inverse of the matrix
     data <- x$getmatrix()
     inverse <- solve(data)
     
     ## then we cached the value of the inverse
     x$setinverse(inverse)
     
     ## Finally, we return the value of the calculated inverse
     inverse
     
}
