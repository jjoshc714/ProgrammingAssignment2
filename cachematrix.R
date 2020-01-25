## The functions cache the inverse of a matrix, which may be more useful than computing 
## multiple matrices over and over. 

## This function stores a matrix and computed inverse into a separate environment.

makeCacheMatrix <- function(x = matrix()){
   i <- NULL                     #creates empty object "i" as placeholder for inverse
   set <- function(y) {          #set matrix as special object in separate environment
      x <<- y
      i <<- NULL
   }
   get <- function() x           #gets result of set
   setinverse <- function(solve) i <<- solve #set "i" as matrix inverse 
   getinverse <- function() i    #gets result of setinverse
   list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}                                #creates list containing previous four functions

## This function returns solved "i" from previous function unless if empty; recomputes

cacheSolve <- function(x, ...) {
   i <- x$getinverse()           #stores "i" as computed inverse from previous function
   if(!is.null(i)) {             #if "i" not empty, return message and value
      message("getting cached data")
      return(i)
   }
   data <- x$get()               #store matrix from separate environment into "data"
   i <- solve(data, ...)         #store inverse of "data" into "i" 
   x$setinverse(i)               #store "i" back into separate environment as "setinverse"
   i
}
