## R functions are able to cache potentially time-consuming computations. 
## For example, taking the inverse of a matrix is typically a costly computation. 
## If the contents of a matrix are not changing, it may make sense to cache the value of the inverse so that when we need it again, it can be looked up in the cache rather than recomputed. 
## The functions below take advantage of the scoping rules of the R language and how they can be manipulated to preserve state inside of an R object.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  #Set i equal to null when function is called
  i <- NULL
  #Set value of x and make i null
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  #Return the value of x
  get <- function() x
  #Set value of i
  setinv <- function(inv) i <<- inv
  #Return the value of i
  getinv <- function() i
  #Return list of functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  #Get inverse value and set to i
  i <- x$getinv()
  #Check if i is not null, if so return i
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  #Set data equal to x value
  data <- x$get()
  #Calculate inverse of x value
  i <- solve(data, ...)
  #Cache calculated inverse
  x$setinv(i)
  #Print inverse matrix
  i
}
