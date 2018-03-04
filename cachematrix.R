## The first function takes a matrix x and creates an object that can cache the inverse
## The second function returns the inverse of x from the cache or sets it up

## makeCacheMatrix creates a matrix "x" capable of inversion

makeCacheMatrix <- function(x = matrix()) {
 
  inv <- NULL ## Reset the matrix inverse to Null
  
  ## This is a transient function to set the matrix
  ## Clear the old cache
  
  set <- function(y) {
     x <<- y # Set the value of the matrix
    inv <<- NULL  # clear the old matrix
  }
  
  get <- function() x # Function that "gets" the matrix x
  setInverse <- function() inv <<- solve(x) # function that calculates the inverse if there is no cached inv
  getInverse <- function() inv # get the inverse & to be called by cacheSolve
  
  #This returns a list of the four functions set, get, setInverse and getInverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## This works with makeCacheMatrix 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

    inv <- x$getinverse() # Get inverse from makeCacheMatrix provided that it exists
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv) # If inverse exists in cache return and leave function
  }
    
  #This occurs if the inv is Null
  data <- x$get() # Call to get in makeCacheMatrix to obtain matrix
  inv <- solve(data) # calculate the inverse
  x$setinverse(inv) # Put the inverse into the cache
  inv # Retrun the inverse QED
  
  
}
