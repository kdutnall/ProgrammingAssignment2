## Put comments here that give an overall description of what your
## functions do

# Generating a matrix to be inverted using the cacheSolve function
makeCacheMatrix <- function(x = matrix()) {
  # Initialising inv to be "NULL" as a default
  inv <- NULL
  
  # New function on y which generates a variable called x as a copy and puts both x and y in cache
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  get <- function()x
  
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv 
  
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}





# Do we already have the matrix held cache?
cacheSolve <- function(x, ...) {

  ## Set inv to be a matrix of the matrix x
  # If inv does exist then return the cached matrix, display it, and exit out of the function
  
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("Exisiting Cached Data")
    return(inv)
  }
###############################################################################  
  # BUT ...
  # If the inverse matrix does not exist or match the entered one then create it
  # "solve" will return the direct inverse of the given matrix
  # Setting x to be the "new" inversed matrix
  # Return the inverted matrix
  
  mat <- x$get()
  inv <- solve(mat,...)
  x$setInverse(inv)
  inv
}
