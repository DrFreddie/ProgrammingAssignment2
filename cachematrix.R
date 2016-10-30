## Put comments here that give an overall description of what your
## functions do
## The makeCacheMatrix will the object called "matrix" that will be able to cache its inverse
## THe cacheSolve function will use it to calculate the inverse using the cached matrix
## Write a short comment describing this function

## makeCachematrix function is a function that is designed to do the following
## Both set and get the value of the matrix
## With that, set and get the value of the inverse of the specified matrix


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y)
  {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(i) m <<- solve(x)
  getInverse <- function() m
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## The following function "cacheSolve" calculates the inverse of the special "matrix" 
## It uses the !is.null function to define whether there is already calculated value. If so, it gets the inverse from the cache and skips the
## computation. Otherwise, it calculates the inverse of the data and sets the inverse
## matrix in the cache via the setInverse function.
cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m))
  {
    return(m)
  }
  m <- solve(x$get())
  x$setInverse(m)
  m
}

