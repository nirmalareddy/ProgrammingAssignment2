## The makeCacheMatrix function enables us to create an object which initialises a matrix and provides a few fucntions to access the matrix as well as the  
## the given matrix's invertible version.
## The cacheSolve fucntion is used to calculate the invertible matrix of the given matrix using the solve function. It returns the cached value if it is 
## already available. Otherwise it calculates it using the solve function.

## makeCacheMatrix function takes a invertible matrix an the input and returns a set of four functions which can be used to access the matrix and its inverse if its set.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       getsolve = getsolve,
       setsolve = setsolve)  
}


## cacheSolve function returns the inverted matrix of an matrix which is initilaised using the makeCacheMatrix function. If the value is already computed then 
## it returns the cached value. Otherwise it is computed. If cached value is returned then a message is printed on the screen.

cacheSolve <- function(x,...) {
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
