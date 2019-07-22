## Creates a matrix "object" capable of storing it's inverse

makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  set <- function(y) {
    x <<- y
    I <<- NULL
  }
  get <- function() x
  setInv <- function(Inv) I <<- Inv
  getInv <- function() I
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## Returns the inverse of a "CacheMatrix", calculating and caching it if it hasn't been done before

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  I <- x$getInv()
  if(!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  data <- x$get()
  I <- solve(data, ...)
  x$setInv(I)
  I
}
