## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
 ## @x: an invertible matrix
     ## result - a list containing functions to
     ##              a) set the matrix
     ##              b) get the matrix
     ##              c) set the inverse
     ##              d) get the inverse
     ##         this list is used as the input to cacheSolve()
     
     invs <- NULL
     set <- function(y) {
         x <<- y
         invs <<- NULL
     }
     get <- function() x
     setinverse <- function(inverse) invs <<- inverse
     getinverse <- function() invs
     list(set = set,
          get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## @x: output of makeCacheMatrix()
     ## result: inverse of the original matrix input to makeCacheMatrix()
     
     invs <- x$getinverse()
     if (!is.null(invs)) {
         message("getting cached data")
         return(invs)
     }
     data <- x$get()
     invs <- solve(data, ...)
     x$setinverse(invs)
     invs
}
