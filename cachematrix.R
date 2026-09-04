## The functions store a matrix and its inverse.
## If A^-1 is already computed it does not compute it again.

## Creates an object with two variables and four methods:
## x: The matrix of interest
## m: The cached inverse of x
## set: Changes the matrix and deletes the old inverse
## get: Retrieves the matrix
## setinverse: Stores the inverse
## getinverse: retrieves the stored inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m

  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Checks if A^-1 is already computed.
## If not, computes A^-1 and caches A^-1

cacheSolve <- function(x, ...) {

  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)

  m
}

## testing

mat <- matrix(c(3,2,5,6,5,4,9,8,7), 3,3)
mat_cache <- makeCacheMatrix(mat)
cacheSolve(mat_cache)
cacheSolve(mat_cache)
solve(mat)

# Looks good :)
