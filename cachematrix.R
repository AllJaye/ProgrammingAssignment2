## A pair of functions that cache the inverse of a matrix to reduce the need for costly computation

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL  ## initializes cached value to Null
  set <- function(y) { ##creates the matrix in the current working environment
    x <<- y
    cache <<- NULL
  }
  get <- function() x ##get the value of the matrix
  setinverse <- function(inverse) cache <<- inverse ##invert the matrix and store it in cache
  getinverse <- function() cache ##get inverted matrix from cache
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) ##return the created functions in the current working environment
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  cache <- x$getinverse() ##get inverse of the matrix from cache
  if(!is.null(cache)) { ##if matrix exists, return it, else create the matrix in the working env.
    message("getting cached data.")
    return(cache)
  }
  matrix <- x$get() ##create the matrix
  cache <- solve(matrix, ...) ##set and return the inverse
  x$setinverse(cache) ##set inverted matrix in cache
  return(cache) ##display matrix 
}
