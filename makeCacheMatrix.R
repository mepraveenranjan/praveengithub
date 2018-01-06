## A pair of functions that cache the inverse of a matrix

## Created a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
      #set initial value of the inverse
      inv <- NULL
     
       #function #1: set value of matrix
      set <- function(matrix) {
            x <<- matrix
            inv <- NULL
      }
      
      #function #2: get value of matrix (return matrix)
      get <- function() {
            x
      }
     
      #function #3: set value of matrix inverse
      setInverse <- function(inverse) {
            inv <<- inverse
      }
      
      #function #4: get value of matrix inverse (return inverse)
      getInverse <- function() {
            inv
      }
      
      #return list of functions
      list(set = set, get = get, 
           setInverse = setInverse, getInverse = getInverse)
}


## Compute a special "matrix" returned by makeCacheMatrix
## if inverse has already been calculated (and matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
      #set initial value of the inverse
      inverse <- x$getInverse()
      
      #check if inverse has already been calculated and if so, skip computation
      if (!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      
      #get matrix 'x'
      data <- x$get()
     
      #solve inverse of 'x'
      inverse <- solve(data)
      
      #set inverse
      x$setInverse(inverse)
      
      #return inverse
      inverse
}      