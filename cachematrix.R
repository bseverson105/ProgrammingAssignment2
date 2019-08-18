## makeCacheMatrix function creates a matrix object that can cache 
## its inverse and cacheSolve either determines inverse or pulls from cache

## Creates a matrix object capable of caching its inverse
makeCacheMatrix <- function(x = matrix()) {
  x_inv <- NULL
  set <- function(y) {
    x <<- y
    x_inv <<- NULL
  }
  get <- function() x
  set_inv <- function(solve) x_inv <<- solve
  get_inv <- function() x_inv
  list(set = set, get = get,
       set_inv = set_inv,
       get_inv = get_inv)
}

## Either pulls matrix inverse from cache or calculates it and stores it in cache
cacheSolve <- function(x, ...) {
  x_inv <- x$get_inv()
  if(!is.null(x_inv)) {
    message("getting cached data")
    return(x_inv)
  }
  data <- x$get()
  x_inv <- solve(data)
  x$set_inv(x_inv)
  x_inv
}