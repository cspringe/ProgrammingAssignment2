## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
## retrieves the inverse from the cache.

## Create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
     ## Initialize the s variable to NULL (doesn't yet have a real value)
     s <- NULL
     
     ## Define the set function, which caches the original matrix itself and initializes or resets s to NULL (sets both of these in 
     ## the parent environment, not just the local function)
     set <- function(y) {
          x <<- y
          s <<- NULL
     }
     
     ## Define the get function, which simply returns the data stored in the x variable
     get <- function() x
     
     ## Define the setInverse function, which caches (in the s variable) the matrix inverse that was passed via the solve parameter
     setInverse <- function(solve) s <<- solve
     
     ## Define the getInverse function, which returns the value of the matrix inverse, stored in the s variable
     getInverse <- function() s
     
     ## Create a list of the functions defined above
     list(set = set, get = get,
          setInverse = setInverse,
          getInverse = getInverse)
     
}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
     ## Call the function to get the matrix inverse of x, if one has previously been cached
     s <- x$getInverse()
     
     ## If the getInverse function didn't return NULL, that means it was cached and the inverse doesn't need to be solved again
     ## Return the cached value (stored in the s variable) and skip execution of the rest of this function
     if(!is.null(s)) {
          message("Getting cached data")
          return(s)
     }
     
     ## Get the value of the x variable and store it in the data variable
     data <- x$get()
     
     ## Calculate the inverse of the matrix in the data variable
     s <- solve(data, ...)
     
     ## Cache the matrix inverse, currently stored locally in the s variable
     x$setInverse(s)
     
     ## Return the value of s, which should now be the matrix inverse of x
     s
}
