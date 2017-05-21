## These two functions construct a matrix wrapper object, and then
## use that object to calculate / cache the inverse of the matrix

## makeCacheMatrix creates a wrapper object for the input square, invertible matrix
## the wrapper object is a list that has the following four functions
## 1. set : sets to the value of the input matrix
## 2. get : gets the input matrix
## 3. setinverse : set the inverse of the original matrix
## 4. getinverse : get the inverse of the original matrix

makeCacheMatrix <- function(x = matrix()) {
      ## Initialize the inverse to NULL
      matInv <- NULL
      
      ## Set the value of the original matrix, and re-NULL the 
      ## inverse if need be
      set <- function(y) {
            x <<- y
            matInv <<- NULL
      }
      ## Get the original matrix back out
      get <- function() x
      # Set the inverse of the matrix
      setinverse <- function(inv) matInv <<- inv
      
      #Get the inverse of the matrix
      getinverse <- function() matInv
      
      # Return the list of our member functions
      list(set = set,
           get = get,
           setinverse = setinverse,
           getinverse = getinverse)      
}      

## cacheSolve calculates the inverse of the input (square, invertible) 'matrix'
## object previously created by makeCacheMatrix() 
## If the matrix was already calculated and cached, the cached value is returned
## after a message "getting cached data"
## Otherwise the inverse is calculated and cached for later use

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      matInv <- x$getinverse()
      ## Check if we have already calculated the inverse
      ## If so, just return it
      if(!is.null(matInv)){
            message("getting cached data")
            return(matInv)
      }
      ## If we have not already calculated the inverse
      ## Get the original matrix and store it in 'data'
      data <- x$get()
      ## We are assuming that the matrix is both square and invertible
      ## Call 'solve' to calculate the inverse
      matInv <- solve(data)
      ## Save this inverse in the cache
      x$setinverse(matInv)
      ## Return the inverse of the matrix
      matInv
}
