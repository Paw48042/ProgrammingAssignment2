## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix function is a getter and setter for matrix "x",
## and getter and setter for inverse of "x".
## The makeCacheMatrix function will return list of functions(getter and setter that we write), 
## so the cacheSolve function could use them.
makeCacheMatrix <- function(x = matrix()) {
  
  inverseMatrix <- NULL 
  
  # Setter function
  set <- function(y = matrix()){
    x <<- y 
    inverseMatrix <<- NULL
  }
  
  # Getter function
  get <- function(){
    return(x)
  }
  
  # Set Inverse of matrix
  setInverse <- function(i){
    inverseMatrix <<- i
  }
  # Get Inverse of matrix
  getInverse <- function(){
    return(inverseMatrix)
  } 
  
  # Return the list of function 
  return(list(
    set = set, 
    get = get,
    setInverse = setInverse,
    getInverse = getInverse
  ))
  
}


## Write a short comment describing this function
## cacheSolve function will calculate the inverse of matrix, 
## if that have already been solve, cacheSolve will get the answer from the existing variable.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  # Get inverse matrix that may cache
  inverseMatrix <- x$getInverse()  
  # Check if there ever been a data stored for this yet?
  if(!is.null(inverseMatrix)){
    
    message("Get Cache Data") 
    return(inverseMatrix)
  } 
  
  # If data is NULL, 
  data <- x$get() 
  # Try out the calculation with try catch so we handle error gracefully
  tryCatch({
    inverseMatrix <- solve(data) 
    # set inverse matrix
    x$setInverse(inverseMatrix)
    # Return inverse matrix
    return(inverseMatrix)
  },
  error = function(e){
    # Print the error message
    message(e)
  })
 
}
