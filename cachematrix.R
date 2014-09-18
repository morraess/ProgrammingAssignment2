## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function takes a matrix as input and prepares all the functions needed to store the inverse in cache. 
## It provides all the setter and getter methods to work with the inverse cache
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL ##initialise m to null 
  orgData <- NULL ##initialise variable to capture orgData
  
  ## prepare set function
  set <- function(y) {
    x <<- y ## store matrix
    ##print(x)
    m <<- NULL ##each call to the set-function deletes the cache
  }
  
  ## prepare get function
  get <- function() x ## get inverse 
  
  ## set the inverse of the given matrix and write it into the cache 
  setinverse <- function(inverse) m <<- inverse
  
  ## retrieve the inverse and return it
  getinverse <- function(){
    ##check whether matrix was changed
    m
  }
  
  ## prepare the list of functions as return value
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  
}


## Write a short comment describing this function
## This function returns the inverse of the matrix. In case it was not stored in cache already, it computes the inverse first and stores it.
## In later calls to the function, the cache inverse will be retrieved and returned.
cacheSolve <- function(x) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse() ## get the cached inverse if possible
  
  ## check whether there was a cached value in m 
  if(!is.null(m)) { ## check whether m contain the cached value 
    ## alright, the cached value is there, retrieve it
    message("getting cached data")
    return(m)
  }
  
  ## oh no, the cached value is not there... so we need to compute it here
  data <- x$get() ## get the original data 
  m <- solve(data) ## compute the inverse matrix of the data provided
  x$setinverse(m) ## store the inverse to cache
  m ##return the inverse to the caller
}