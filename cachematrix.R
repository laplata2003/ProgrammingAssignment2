## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##
## Creates a special 'matrix' which is able to store its inverse.
##
makeCacheMatrix <- function(x = matrix()) {

    # Initialy, inverse matrix is not defined
    inverseMatrix <- NULL
    
    # Sets the matrix value
    set <- function(y) {
        x <<- y
        inverseMatrix <<- NULL
    }
    
    # Returns the matrix value
    get <- function() {
        x
    }
    
    # Sets the inverse matrix value
    setInverse <- function(inverse) {
      inverseMatrix <<- inverse
    }
    
    # Returns the inverse matrix
    getInverse <- function() {
      inverseMatrix
    }
    
    list(set = set, 
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
  
}


## Write a short comment describing this function

##
## Returns the cached inverse of a given matrix if defined, otherwise calculates, sets and
## returns it.
##
cacheSolve <- function(x, ...) {
    
    # Verifies if the inverse matrix was cached 
    inverseMatrix <- x$getInverse()
    if (!is.null(inverseMatrix)) {
      
        # Returns the cached inverse matrix
        message("getting cached data")
        return(inverseMatrix)
    
    }
  
    # Inverse matrix was not cached, then calculates and sets it!
    matrix <- x$get()
    inverseMatrix <- solve(matrix, ...)
    x$setInverse(inverseMatrix)
  
    # Returns the inverse matrix
    inverseMatrix
  
}
