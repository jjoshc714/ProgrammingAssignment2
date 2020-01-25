## The functions cache the inverse of a matrix, which may be more useful than computing 
## multiple matrices over and over. 

## This function stores four functions: 1. set matrix as special object in a separate
## environment, 2. get "1.", 3. set "i" as matrix inverse, and 4. get "i" in a list.

makeCacheMatrix <- function(x = matrix()){
      i <- NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) i <<- solve
      getinverse <- function() i
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function returns solved i from previous function unless empty: then gets back
## the stored matrix, solves its inverse as "i", and stores it back.

cacheSolve <- function(x, ...) {
      i <- x$getinverse()
      if(!is.null(i)) {
            message("getting cached data")
            return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setinverse(i)
      i
}
