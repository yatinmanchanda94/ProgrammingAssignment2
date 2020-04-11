## R ProgrammingAssignment2 Caching the Inverse of a Matrix

## This function creates a special “matrix” object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  t <- NULL
  set <- function(s){
    x <<- s
    t <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) t <<- inverse
  getInverse <- function() t 
  list(set = set, get = get, 
     setInverse = setInverse, 
     getInverse = getInverse)
}

## This function computes the inverse of the special “matrix” returned 
## by makeCacheMatrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  t <- x$getInverse()
  if(!is.null(t)){
    message("getting cached data")
    return(t)
  }
  mat <- x$get()
  t <- solve(mat,...)
  x$setInverse(t)
  t
}

##Test Data

test<- matrix(15:18,2,2)
testCache <- makeCacheMatrix(test)
testCache$getInverse()
cacheSolve(testCache)
cacheSolve(testCache)
testCache$getInverse()
