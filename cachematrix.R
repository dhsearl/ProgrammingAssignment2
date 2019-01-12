## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function. Sure:
## makeCacheMatrix creates a special instance of a matrix
##  and since it returns a list of named attributes we can
##  access them with the $ sign.  Three can be used at the 
##  command line: matrix1$set, matrix1$get, matrix1$getinverse
##  the fourth command is only used by cacheSolve

##  Set - re-initializes the matrix and clears any associated
##    cached values (stored at m).  Same code as when you first
##    create a special matrix with the function

##  Get - simply returns the object stored at x

##  Setinverse - used by cache solve & stores solution at the 
##    variable m in the parent environment

##  Getinverse - returns m or says nothing has been cached.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

## This function 1st checks to see if the matrix it was passed
## (x) has an inverse cached - if so it returns it.  If not it
## passes the value of (x) to data, solves/returns inverse of (data)
## and stores it in the special matrix (x) it was passed.
## it then returns the inverse to the command line too.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
