## This function is designed to cache the inverse of a square matrix
## in order to save time of the computation

## Note: the input matrix must be square and invertable

## The first part is to convert the matrix into a special form
## by applying a square matrix to the makeCacheMatrix function

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  
  ## list of sub functions that can be used in makeCacheMatrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

## The second part is to return the inverted martix
## The matrix must be in a form of makeCacheMatrix() before being cacheSolve() 

cacheSolve <- function(x, ...) {
        
  ## First, find the cache data to save computation time
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## If the cache data is not found, invert the matrix and cache it for later
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
  
}

## End of the function. Thank you for grading!