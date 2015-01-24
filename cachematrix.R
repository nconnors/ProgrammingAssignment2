## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special list that a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse of the matrix
# 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    
    # initialize m to store the inverse later
    m <- NULL
    
    # define the set function to return the input matrix
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    
    # define the get function to return the matrix
    get <- function() x
    
    # define the setinverse function to calculate the inverse of the input matrix
    setinverse <- function(solve) m <<- solve
    
    # define the getinverse function to retrieve the calculated inverse
    getinverse <- function() m
    
    # return the list of functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  }
}


## Write a short comment describing this function
## This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
  
    ## Retrieve the inverse
    m <- x$getinverse()
    
    ## Check if the inverse has been calculated already, if 
    ## it has, simply retrieve it
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    
    ## If it has not been calculated already , i.e. m is NULL
    ## then get the matrix and do the calculation
    data <- x$get()
    m <- solve(data, ...)
    
    ## Once the inverse is calculated, set it in the cache for next time
    x$setinverse(m)
    
    ## Return the inverse
    m
  }
}
