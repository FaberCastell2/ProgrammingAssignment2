## The script contains two functions makeCacheMatrix() and cacheSolve()
## These functions cache the inverse of a matrix

## The following function creates a special object that stores a matrix and caches its inverse

makeCacheMatrix <- function(x = matrix()) {
  invm <- NULL
  set <- function(y) {
    x <<- y        # Set the value of the matrix
    invm <<- NULL     # Invalidate the cached inverse
  }
  get <- function() x  # Get the value of the matrix
  setInv <- function(inverse) invm <<- inverse  # Set the cached inverse
  getInv <- function() invm  # Get the cached inverse
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## The following function computes the inverse of the matrix returned by makeCacheMatrix above. If the inverse has already been calculated, it will be returned from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invm <- x$getInv()  # Check if inverse is already cached
  if (!is.null(invm)) {
    message("getting cached data")
    return(invm)  # Return cached inverse
  }
  data <- x$get()
  invm <- solve(data, ...)  # Compute the inverse if not cached
  x$setInv(invm)  # Cache the computed inverse
  invm  # Return the computed inverse
  }

# Example usage
mat <- makeCacheMatrix(matrix(sample(1:9),3))
cacheSolve(mat)  # This will compute the inverse
cacheSolve(mat)  # This will retrieve the cached inverse
mat$set(matrix(sample(1:16),4))
cacheSolve(mat)  # This will compute the new inverse
