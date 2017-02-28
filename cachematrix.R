## Below are two functions that are used to create a special object that stores a 
## matrix vector and cache's its inverse.

## The first function, makeVector creates a special "vector", which is 
## really a list containing a function to
## set the value of the vector
## get the value of the vector
## set the value of the mean
## get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
  iv <- NULL
  set <- function(y) {
    x <<- y
    iv<- NULL
}
  get <- function() x
  setiv <- function(solve) iv <<- solve
  getiv <- function() m
  list(set = set, get = get,
       setiv = setiv,
       getiv = getiv)
}


## The following function calculates the inverse of the special "vector" 
## created with the above function. However, it first checks to see if the inverse
## has already been calculated. If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse

cacheSolve <- function(x, ...) {
    m <- x$getiv()
    if(!is.null(iv)) {
      message("getting cached data")
      return(iv)
    }
    data <- x$get()
    iv <- solve(data, ...)
    x$setiv(iv)
    iv
  }
