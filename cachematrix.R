## R Programming
## Programming Assignment 2: Caching the Inverse of a Matrix
## by Joanna Widjaja (jo.widjaja@gmail.com)
## Jan 18, 2015

## These 2 functions work together. The first allows users to create a "matrix" of 
## functions, and pass it to the second function that will compute the inverse of 
## the matrix.

## makeCacheMatrix is a function that creates a special "matrix" object 
## that can cache its inverse..

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setcache <- function(cache) m <<- cache
  getcache <- function() m
  list(set = set, get = get,
       setcache = setcache,
       getcache = getcache)
}


## cacheSolve is a function that computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getcache()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setcache(m)
  m
}