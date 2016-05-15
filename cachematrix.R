## There are two functions in this file to demonstrate lexical scoping in R.
## Calculation of an inverse of a matrix is used as an example.
## Calculation of an inverse of a matrix is usually a expensive computation.
## In this example an inverse of a matrix is calculated and cached.
## This inverse is retrieved from cache provided the matrix has not chnaged.

## makeCacheMatrix creates a list containing a function to
## a. Set the value of the matrix
## b. Get the values of the matrix
## c. Set the value of the inverse of the matrix
## d. Get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cachesolve returns the inverse of the matrix.
## The function first checks if the inverse is already calculated.
## If it is, and the matrix has not changed, it gets the inverse from cache.
## If not, then it calculated the inverse and returns the value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
}
