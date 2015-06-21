## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix2 <- function(x = matrix()) {
  m <- x$getmatrix ()
  if (!is.null(m)){
    message("getting cache")
    return (m)
  }
  data <- x$get()
  m <- solve(data)
  x$setmatrix(m)
  m
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if (!is.null(m)){
    message("getting cache")
    return (m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
  
}

makeCacheMatrix3 <- function(x = matrix(), size) {
  xR <- runif(size*size, 1, 100)
  x <- matrix(xR, nrow = size, ncol = size)
  
}


makeCacheMatrix <- function(x = matrix() ) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve(x)
  getsolve <- function() m
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
  
}