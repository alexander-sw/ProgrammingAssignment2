
### Put comments here that give an overall description of what your
### functions do

### Write a short comment describing this function:

### This function, makeCacheMatrix creates a special "matrix" that cache 
### its inverse into memory.
### Firstly - Set the value of the matrix
### Secondly - Get the value of the matrix
### Thirdly - Set the value of the inverse
### Fourthly - Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse) m <<- inverse
  get_inverse <- function() m
  list(set = set,get = get,set_inverse = set_inverse,get_inverse = get_inverse)
}


## Write a short comment describing this function:

## This function, cacheSolve calculates the inverse of the special "matrix" 
## created from the above function, makeCacheMatrix.
## First it checks if the inverse has already been calculated, then gets 
## the cached inverse. Otherwise solve the inverse of the marix and set the value 
## of the inverse in the cache via the function, set_inverse.   

cacheSolve <- function(x, ...) {
  m <- x$get_inverse()
  if (!is.null(m)) {
    message("Getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$set_inverse(m)
  m
}



  
