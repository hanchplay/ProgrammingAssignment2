## The two functions create a special matrix object that cache its inverse

## function makeCacheMatrix to create special matrix object

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) m <<- inv
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## function cacheSolve to check if matrix inverse has already been calculated,
## if not, calculate the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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

##example usage
data <- matrix(runif(9, min = 0, max = 1), nrow = 3, ncol = 3)
k <- makeCacheMatrix(data)
k$get()
invk <- cacheSolve(k)
invk
