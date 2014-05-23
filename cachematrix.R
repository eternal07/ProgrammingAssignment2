## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  
  m <- NULL
  
  ##################################
  # SETTING THE INVERSE OF A MATRIX
  #################################
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  #######################
  # GETTING THE INVERSE OF MATRIX
  ######################
  get <- function() x
  setinverse <- function(inverse) m <<- ginv(inverse)
  getinverse <- function() m
  
  ########################
  # GENERATING THE VECTOR WITH INVERSE
  ###########################
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}




## THis function generates a list of vectors that consists of a matrix and its inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- ginv(data, ...)
  x$setinverse(m)
  m
}



