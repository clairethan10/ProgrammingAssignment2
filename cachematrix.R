## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the inverse as NULL
  
  # Set the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset the cached inverse when the matrix changes
  }
  
  # Get the value of the matrix
  get <- function() x
  
  # Set the cached inverse
  setInverse <- function(inverse) inv <<- inverse
  
  # Get the cached inverse
  getInverse <- function() inv
  
  # Return a list of functions to access and modify the matrix and its inverse
  list(set = set, get = get,
       setInverse = setInverse, 
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()  # Retrieve cached inverse
  
  # If the inverse is already cached, return it with a message
  if (!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  
  # Otherwise, calculate the inverse of the matrix
  data <- x$get()  # Get the matrix from the special object
  inv <- solve(data, ...)  # Calculate the inverse using the solve function
  
  # Cache the calculated inverse
  x$setInverse(inv)
  
  # Return the inverse
  inv
  }
