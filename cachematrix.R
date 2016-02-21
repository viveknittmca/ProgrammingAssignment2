## Caching the Inverse of a Matrix:
## Matrix inversion is a costly computation and there may be some 
## benefit to caching its inverse rather than computing it everytime.
## Below are a set of functions that are used to create a special object  
## which stores a matrix and caches its inverse.

# This function creates a special "matrix" object as well can set its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  # Initializing the inverse matrix to NULL
  inv <- NULL
  
  # setting the actual matrix. Here, inverse is set to NULL
  set <- function(matrix) {
    x <<- matrix
    inv <<- NULL
  }
  
  # getting the actual matrix
  get <- function() x
  
  # setting the inverse of the matrix
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  
  # getting the inverse of the matrix
  getInverse <- function() {
    inv
  }
  
  # returning the list of the above set and get methods
  list (set = set, 
        get = get, 
        setInverse = setInverse,
        getInverse = getInverse
  )
  
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it will return the inverse from its cache

cacheSolve <- function(x, ...) {

  # getting the inverse of x
  inv <- x$getInverse()
  
  # check if inverse is computed. if not then compute
  if ( !is.null(inv) ) {
    message("getting cached data")
    return (inv)
  }
  
  # getting the actual matrix
  m <- x$get()
  
  # computing its inverse using solve() function
  inv <- solve(m, ...) 
  
  # setting the inverse matrix
  x$setInverse(inv)
  
  inv
}
