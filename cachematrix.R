##  This program cache potentially time-consuming computations
##  of inverse of a matrix so no need to recalculate 
##  inverse if call again with same input matrix.

# Function makeCacheMatrix creates a list of functions to 
# set the value of a matrix (set.matrix)
# get the value of a matrix (get.matrix)
# set inverse of the matrix  (set.inverse.matrix)
# get inverse of the matrix  (get.inverse.matrix)

makeCacheMatrix <- function(x = matrix()) {
      
      set.matrix <- function(y) {
            z <<- y
      }
      
      get.matrix <- function() {
            return(x)
      }
      
      set.inverse.matrix <- function(inverse.matrix) {
            m <<- inverse.matrix 
      }
      
      get.inverse.matrix <- function() {
            return(m)
      }
      
      # return list of functions
      list(
            set.matrix = set.matrix, 
            get.matrix = get.matrix,
            set.inverse.matrix = set.inverse.matrix,
            get.inverse.matrix = get.inverse.matrix
      )   
}


# Function cacheSolve returns a inverse of a matrix.
# If input matrix not changed from the previous run
# it returns inverse from cache, otherwise calculates
# returns inverse.

# Note: This program assumes that the matrix supplied (x) 
#       is always invertible as per assignment instructions.

cacheSolve <- function(x, ...) {

      if( !exists("z") ) z <<- NULL
      
      # z is previously saved matrix in cache.
      # if z matrix not identical to current matrix 
      # calculate inverse of this matrix and save into z
      if ( !identical(z , x$get.matrix()) ) {
            message("Calculating inverse of the matrix..")
            matrix.data <- x$get.matrix()
            inverse.matrix <- solve(matrix.data, ...)
            x$set.inverse.matrix(inverse.matrix)
            x$set.matrix(x$get.matrix())
      } 
      else {
            message("Returning cached inverse-matrix...")
            
      }
      
      # return inverse of input matix
      return(x$get.inverse.matrix())
}
