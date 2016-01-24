## The first function makeCacheMatrix, creates a special matrix that contains functions to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. invert and set the value of the matrix
## 4. get the value of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
  x <<- y
  m <<- NULL
}
get <- function() x
setmatrix <- function(solve) m <<- solve
getmatrix <- function() m
list(set=set, get=get,
   setmatrix=setmatrix,
   getmatrix=getmatrix)
}


## cacheSolve function computes the inverse of the matrix returned by makeCacheMatrix.  If 
## the inverse has already been calculated then cacheSolve will return the inverse from the cache.

cacheSolve <- function(x=matrix(), ...) {
    m <- x$getmatrix()
    if(!is.null(m)){
      message("getting cached data")
      return(m)
    }
    matrix <- x$get()
    m <- solve(matrix, ...)
    x$setmatrix(m)
    m
}