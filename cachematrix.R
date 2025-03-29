## This function creates a special kind of matrix that can remember its inverse.
## Instead of recalculating the inverse every time, we can store it and use it later.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # This will hold the inverse once we calculate it
  
  ## This function lets us set a new matrix and clears the cached inverse
  set <- function(y) {
    x <<- y     # Store the new matrix
    inv <<- NULL  # Reset the cached inverse since the matrix changed
  }
  
  ## This function returns the current matrix
  get <- function() x 
  
  ## This function saves the inverse so we don’t have to recalculate it later
  setInverse <- function(inverse) inv <<- inverse 
  
  ## This function returns the cached inverse (if we have one)
  getInverse <- function() inv 
  
  ## Return a list of these functions so we can use them later
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function calculates the inverse of a matrix, but only if needed.
## If we’ve already calculated it before, it just grabs the cached version.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()  # Check if we already have the inverse stored
  
  ## If the inverse is already cached, return it instead of recalculating
  if (!is.null(inv)) {
    message("Getting cached data")  # Let the user know we're using the stored inverse
    return(inv)
  }
  
  ## If there's no cached inverse, calculate it now
  mat <- x$get()  # Get the original matrix
  inv <- solve(mat, ...)  # Compute the inverse
  x$setInverse(inv)  # Store it in the cache for next time
  
  return(inv)  # Return the newly calculated inverse
}
