## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
 inver<- NULL
      set <- function(y) {
            x <<- y
            inver <<- NULL
      }
      get <- function() {x}
      setinverse <- function(inverse){inver <<- inverse}
      getinverse <- function() {inver}
      list(get=get,
           setinverse=setinverse,
           getinverse=getinverse)
}


## This function calculates the inverse of a matrix or, if already done, get the cached data

cacheSolve <- function(x, ...) {
          ## Return a matrix that is the inverse of 'x'
      inver <- x$getinverse()
      if (!is.null(inver)) {
            message("getting cached data")
            return(inver)
      }
      z <- x$get()
      inver <- solve(z, ...)
      x$setinverse(inver)
      inver
}
