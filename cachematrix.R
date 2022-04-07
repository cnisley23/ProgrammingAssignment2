
## Create and cache matix inversions 

## create a "special" matrix that is a list of the following: 
# sets the values of the matrix 
# gets the values of the matrix 
# sets the values of the matrix inversion 
# gets the values of the matrix inversion 

makeCacheMatrix <- function(x = matrix()) {
  
  # initialize a blank object that will contain values of matrix inverse  
  inverse <- NULL 
  
  # set values of "special" matrix to new in working environment  
  set <- function(y) {
    x <<- y 
    inverse <<- NULL 
  }
  
  # get values of matrix 
  get <- function() {x}
  
  set_inverse <- function(inv_matrix) inverse <<- inv_matrix
  get_inverse <- function() inverse
  
  # function output 
  list(
    set = set, get = get, 
    set_inverse = set_inverse, 
    get_inverse = get_inverse
  )
  
}


## function to compute inverse of "special" matrix created by mackeCacheMatrix() 
# IF matrix hasn't changed and inverse has already been computed,
# then return cached inverse and notify 
# else compute inverse  

cacheSolve <- function(x, ...) {
  
  # grab inverse matrix if it exists/cached
  inverse <- x$get_inverse()
  
  # if inverse already exists, notify user and use cached inverse
  if (!is.null(inverse)) {
    message("Invsere already computed, grabbing cached data")
    return(inverse)
  }
  
  # grab data and compute inverse 
  data <- x$get()
  inverse <- solve(data, ...) #"..." arguments can be passed from cacheSolve() input
  
  x$set_inverse(inverse)
  inverse
  
}
