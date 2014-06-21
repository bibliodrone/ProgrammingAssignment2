## Functions makeCacheMatrix, cacheSolve
##
## makeCacheMatrix accepts a matrix object and can cache its inverse in memory after it is intially calculated
## via cacheSolve(); contains getter and setter functions for the matrix and the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(inv) m <<- inv # the inverse of the matrix
  getInv <- function() m  # return the inverted matrix
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## Accepts matrix from makeCacheMatrix, calculates its inverse if it is not already cached, otherwise
## returns inverse matrix from memory cache
## * Assumes a square, invertible matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data") # if inverse has already been calculated, retrieve from cache
    return(m)
  }
  data <- x$get() # Otherwise, call the solve() function now, on the makeCacheMatrix matrix...
  m <- solve(data, ...) 
  x$setInv(m)  #...and call setInv on solve()d m
  m
}
