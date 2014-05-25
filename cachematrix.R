## Put comments here that give an overall description of what your
## functions do

#This function creates a list of methods to set or retrieve a matrix
#or to set and get its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
   get <- function() x
  setInverseMatrix <- function(matrixToInverse) m <<- matrixToInverse
  getInverseMatrix <- function() m
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
       
}


#This function retrieves the inverse of matrix
#either from the cache or compute it.
#its parameter should be makecachematrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x' from the cache
  m <- x$getInverseMatrix()
  if(!is.null(m)) {
    message("getting cached matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInverseMatrix(m)
  return (m)
}
