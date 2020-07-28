## The purpose of below mentioned functions is to save
## the matrix inverse data into the cache memory.

## This function will be used to perform 4 sets of operations on matrix
## get, set, get inverse of provided matrix and set inverse of the matrix

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(matinv) inv <<- matinv
  getinverse <- function() inv

## This will create list of all the operations  
  list(set = set, get = get,
      setinverse = setinverse,
      getinverse = getinverse)
}


## This function will return inverted matrix if it has not already been 
## done. If the same set of data was used earlier then it will return the same.

cacheSolve <- function(x){
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("Getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
