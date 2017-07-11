makeCacheMatrix <- function(x = matrix() ) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function() m <<- solve(x)
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
########   Example Usage  ########  
test <- makeCacheMatrix()
test$set(matrix(1:4,2))
test$get()
test$setInverse()
test$getInverse()
