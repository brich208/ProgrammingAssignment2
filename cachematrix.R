## The following two functions first create a matrix in the first function
## then solve for the inverse of that matrix in the second function. The first
## function allows the user to pull the original matrix (getMat), the inverse
## matrix (getInv), to solve for the inverse (setInv), and pull the inverse
## (getInv). The user can also change the value of an existing matrix (setMat).

## The following functions creates a matrix and also contains a list of 
## functions to retrieve the matrix (getMat), retrieve the inverse matrix 
## (getInv), set the inverse matrix (setInv), and change an exisiting 
## matrix (setMat).

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  setMat <- function(y) {
    x <<- y
    m <<- NULL
  }
  getMat <- function() x
  setInv <- function(solve) m <<- solve
  getInv <- function() m
  list(setMat = setMat, getMat = getMat,
       setInv = setInv,
       getInv = getInv)
}


## The following function first checks to see if the inverse already exists,
## and returns the inverse from the cache if it does. If the inverse does
## not exist in the cache the function uses setInv to solve for the inverse.

cacheSolve <- function(x, ...) {
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached inverse matrix")
    return(m)
  }
  data <- x$getMat()
  m <- solve(data, ...)
  x$setInv(m)
  m
}
