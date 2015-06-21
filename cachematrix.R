## Put comments here that give an overall description of what your
## functions do
# makeCacheMatrix creates a list with for objects:
# set : a function that set the matrix values
# get : a function that get the matrix values
# setinv: a function that computes the inverse of the matrix
# getinv: a function that get the inverse the matrix
# cacheSolve computes the inverse of the matrix that was set with the set functio above.
# if the inverse has been computed already, it get it and return it without recomputing it again.

## Write a short comment describing this function
# makeCacheMatrix <- function(x )
# where x is a matrix 

makeCacheMatrix <- function(x = matrix()) {
         inv <- NULL
         set <- function(y) {
            x <<- y
            inv <<- NULL
         }
         get <- function() x
         setinv <- function(solve) inv <<- solve
         getinv <- function() inv
         list(set = set, get = get,
              setinv = setinv,
              getinv = getinv)
}

## Write a short comment describing this function
# cacheSolve <- function(x, ...) 
# computes the inverse of a matrix x
# if matrix inverse have been computed, it just gets the inverse
# otherwise, computes the inverse and return it
#
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         inv <- x$getinv()
         if(!is.null(inv)) {
         message("getting cached matrix inverse")
         return(inv)
}
         data <- x$get()
         inv <- solve(data,...)
         x$setinv(inv)
         inv
}
