## Summary :
## makeCacheMatrix : creates a special 'matrix' that allows caching of inverse of 'matrix'
## cacheSolve : get the cache of a 'makeCacheMatrix' or calculates and return the inverse if necessary

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
##    if not cached, calculate and return the inverse of the matrix stored in 'x' 
##    IMPORTANT : I've decided for making clear that the parameter for cacheSolve is makeCacheMatrix and
##    not including "..." as it will not make sense for the assignment, given solve(x, ...) 
##    will do something different of simply calculating the inverse of a matrix
cacheSolve <- function(x = makeCacheMatrix()) {
  m <- x$getinverseofmatrix() # get the inverse of the matrix in 'x' 
  
  # if m is null, get the matrix, calculate the inverse, cache the result and return the inverse
  if(is.null(m)) {
    data <- x$get()
    m <- solve(data)
    x$setinverseofmatrix(m)
  }

  m
}
