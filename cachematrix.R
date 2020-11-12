## This function stores will cache a matrix and compute the inverse of the matrix

## makeCacheMatrix will store a cache the matrix. If an invertible matrix is used as the input
## it will return "this matrix is not invertible" 


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL 
  set <- function(y) {
    x <<- y 
    m <<- NULL 
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m 
  list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function will solve the inverse of the matrix, by first looking for the 
## cached matrix. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message ("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
