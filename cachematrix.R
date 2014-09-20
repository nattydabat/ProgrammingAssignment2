## This script defines functions that create an object which 
## store a matrix and calculate and store the inverse of 
## the stored matrix in a cache.

## The naming of variables and functions followed the 
## guideline from Google's R Style Guide which can be found
## https://google-styleguide.googlecode.com/svn/trunk/Rguide.xml


## The following function creates a special object which is
## used to set a cached matrix and its inverse, and retrieve 
## the cached matrix and inverse matrix on request.

makeCacheMatrix <- function(x = matrix()) {
  cached.inverse.matrix <- NULL 
  
  set <- function(y) { 
    cached.matrix <<- y
    cached.inverse.matrix <<- NULL
  }
  
  get <- function() cached.matrix
  SetInverseMatrix <- function(inverse.matrix){
    cached.inverse.matrix <<- inverse.matrix
  }
    
  GetInverseMatrix <- function() cached.inverse.matrix
  
  return(list(set = set, get = get,
              SetInverseMatrix = SetInverseMatrix,
              GetInverseMatrix = GetInverseMatrix))
}


## The function below is used to retrieve the cached
## inverse matrix if it is present, otherwise it will
## compute the inverse of the cache matrix and store it 
## as a cached variable, and return the newly cached inverse
## matrix.

cacheSolve <- function(x, ...) {
  cached.inverse.matrix <- x$GetInverseMatrix()
  
  ## Return a matrix that is the inverse of 'x'
  if(!is.null(cached.inverse.matrix)) {
    message("getting cached data")
    return(cached.inverse.matrix)
  }
  
  cached.matrix <- x$get()
  cached.inverse.matrix <- solve(cached.matrix, ...)
  x$SetInverseMatrix(cached.inverse.matrix)
  return(cached.inverse.matrix)
}
