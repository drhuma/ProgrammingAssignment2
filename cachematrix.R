#  Caching the Inverse of a Matrix
## Make a special matrix, that can cache its inverse

## Below are two functions that are used to create a special matrix and cache's its inverse

#The first function, makeCacheMatrix creates a special "matrix", which is a list of get and set function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

# The Second function can inverse the special "matrix" created with the above function
# If the inverse has already been calculated before, the cached inverse is returned

cacheSolve <- function(x, ...) {
  
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("inverse is cached")
    return(inv)
  } 
  ## Return a matrix that is the inverse of 'x'
  
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinv(inv)
  inv      
}
