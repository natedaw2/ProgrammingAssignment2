#Write the following functions
#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#cacheSolve: This function computes the inverse of the special "matrix"
##returned by makeCacheMatrix above. If the inverse has already been calculated
##(and the matrix has not changed), then the cacheSolve should retrieve the inverse
##from the cache
#Use the solve function to invert; for example, solve(X) returns its inverse

#makeCacheMatrix
makeCacheMatrix <- function(x = matrix()) {
  ## x = square, invertible matrix
  ## return = a list containing functions to:
  ## 1. set the matrix
  ## 2. get the matrix
  ## 3. set the inverse
  ## 4. get the inverse
  ## this list is used as the input to cacheSolve()
  
  inv_mtrx <- NULL
  set <- function(y) {
    x <<- y
    inv_mtrx <<- NULL
  }
  get <- function() x
  setinvmtrx <- function(solve) inv_mtrx <<- solve
  getinvmtrx <- function() inv_mtrx
  list(set = set, get = get,
       setinvmtrx = setinvmtrx, 
       getinvmtrx = getinvmtrx)
}

#cacheSolve
cacheSolve <- function(x, ...) {
  ## x = output of makeCacheMatrix()
  ## return = inverse of the original matrix input
  
  inv_mtrx <- x$getinvmtrx()
  
  # if the inverse has already been calculated
  if(!is.null(inv_mtrx)) {
    
    # get it from the cache and skip calculation
    message("getting cached data")
    return(inv_mtrx)
  }
  
  # otherwise calculates the inverse
  mtrxdata <- x$get()
  inv_mtrx <- solve(mtrxdata, ...)
  
  # sets the value of the inverse in the cache
  x$setinvmtrx(inv_mtrx)
  return(inv_mtrx)
}
