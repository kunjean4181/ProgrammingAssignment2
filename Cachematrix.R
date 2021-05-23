# Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

makeCacheMatrix <- function(a = matrix())
{
  inverse <- NULL
  set <- function(y)
  {
    a <<- b
    inverse <<- NULL
  }
  get <- function() {a}
  setinv <- function(inv) {inverse <<- inv}
  getinv <- function() {inverse}
  list(set =set, get =get, setinv = setinv, getinv = getinv)
}
## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cachesolve <- function(a, ...)
{
  inverse <- a$getinv()
  if(!is.null(inverse)){
    message(" getting data from the cache")
    return(inverse)
    
  }
  mati <- a$get()
  inverse <- solve(mati, ...)
  a$setinv(inverse)
  inverse
}
