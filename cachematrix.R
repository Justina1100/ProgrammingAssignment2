## These functions create a special matrix object that can cache its
## inverse and retrieve the cached inverse when available, avoiding
## repeated computation.

## Creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
    
  get <- function() x
  
  setinverse <- function(inverse) inv <<- inverse
  
  getinverse <- function() inv
  
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
    
  )
}


## computes the inverse of the special matrix or returns the
## cached inverse if it has already been calculated.

cacheSolve <- function(x, ...) {
  
  inv <- x$getinverse()

  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  
  ## Return a matrix that is the inverse of 'x'
  inv <- solve(data, ...)
  
  x$setinverse(inv)
  
  inv
}

