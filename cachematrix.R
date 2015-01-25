## The following functions implement a container object 
## for storing a matrix and its inverse

## This function creates a list containing access methods 
## to get and set the matrix and its inverse in the defining environment

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setinv <- function(inverse) {
    inv <<- inverse
  }
  
  getinv <- function() {
      inv
  }
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function calculates the inverse of the matrix, 
## if the inverse is not already cached

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
