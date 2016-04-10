## Functions for holding a matrix and calculating and caching its inverse.
## Includes: 
##    makeCacheMatrix(x) to construct a wrapper arround matrix x
##    cacheSolve(mcm) to create / return a cached inverse of the matrix

## Tests:
## 1) Confirm that we solve for the matrix (matrix * inverse = identity matrix)
# > m <- makeCacheMatrix(matrix(c(1,2,3,4),2,2))
# > n <- cacheSolve(m)
# > m$get()
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > n
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > m$get() %*% n
# [,1] [,2]
# [1,]    1    0
# [2,]    0    1


## 2) Confirm caching works
# > m <- makeCacheMatrix(matrix(c(1,2,3,4),2,2))
# > n1 <- cacheSolve(m)
# > n2 <- cacheSolve(m)
# getting cached data
# > identical(n1,n2)
# [1] TRUE


## Contruct a makeCacheMatrix 
## Args;
##   x = matrix() to wrap within the "matrix" object
## Return: 
##   "matrix" object:
##       matrix$get() - get the internal matrix 
##       matrix$set(x) - set the internal matrix to x
##       matrix$getinverse()  - get the internal inverse matrix
##       matrix$setinverse(x) - set the internal inverse matrix to be x
##       matrix$clearcache(x) - explicit method to clear the cache

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  set <- function(y) {
    x <<- y
    # implicitly clear the cache of the inverse of the matrix
    inverseMatrix <<- NULL 
  }
  get <- function() x
  setinverse <- function(inverse) inverseMatrix <<- inverse
  getinverse <- function() inverseMatrix
  clearcache <- function() inverseMatrix <<- NULL
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse, 
       clearcache = clearcache)
}


## Return an inverse matrix cached / calculated using solve on makeCacheMatrix
## Args:
##   x = "matrix" object created by a call to makeCacheMatrix()
##   ... = arguments passed to solve(...)

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  
  ## Return a matrix that is the inverse of 'x'
  return(m)
}
