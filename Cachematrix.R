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
