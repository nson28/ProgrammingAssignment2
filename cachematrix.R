## This program will create a  matrix stored in a cache and creates an inverse for the  
## matrix created

##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse_matrix <- NULL
  set_matrix <- function(mat_arg){
    m <<- mat_arg
    inverse_matrix <<- NULL
  }
  
  get_matrix <- function() m
  
  set_inverse <- function(inverse_mat) inverse_matrix <<- inverse_mat
  
  get_inverse <- function() inverse_matrix
  
  list(set = set_matrix, get =  get_matrix, setinv = set_inverse, getinv = get_inverse )
  
  

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinv()
  if(!is.null(inverse)){
    message("getting cached result")
    return(inverse)
  }
  
  data <- x$get()
  inverse <- solve(data,...)
  x$setinv(inverse)
  inverse
}

