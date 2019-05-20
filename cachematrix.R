## Put comments here that give an overall description of what your
## functions do

#The makeCacheMatrix and cacheSolve are used in tandem to calcuate and store 
#the solution to a matrix for the use of minimizing the repeated calculations of the same matrix

## Write a short comment describing this function

#Given a matrix(otherwise default matrix) return a list of operations centered around getting and setting
#the matrix and its solution in a cache
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function(solve) m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function

#Returns the inverse of a selected matrix operations list, calculates the matrix 
#based on whether it has or has not been cached
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
