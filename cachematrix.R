## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix : creates a special 'matrix', which is really a list containing the following functions
##  get() = get the value of the matrix
##  set() = set the value of the matrix
##  getinverseofmatrix() = get the value of the inverse of the matrix
##  setinverseofmatrix(inv) = set the value of the inverse of the matrix as inv
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverseofmatrix <- function(inversem) m <<- inversem
  getinverseofmatrix <- function() m

  list (set = set, get = get, 
        setinverseofmatrix = setinverseofmatrix, 
        getinverseofmatrix = getinverseofmatrix)
}


## cacheSolve : returns the inverse of matrix cached in a special list (makeCacheMatrix) OR
##    if not cached, calculates and return the inverse of the matrix stored in 'x' 
cacheSolve <- function(x = makeCacheMatrix()) {
  m <- x$getinverseofmatrix() # get the inverse of the matrix in 'x' 
  
  # if m is not null, it has been cached before, returns cache, end.
  if(!is.null(m)) return(m)
  
  # else, get the matrix, calculates the inverse, cache the result and return the inverse
  data <- x$get()
  m <- solve(data)
  x$setinverseofmatrix(m)
  m
}
