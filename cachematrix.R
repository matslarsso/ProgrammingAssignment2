##  In this example the <<- operator is used to assign a value 
##  to an object in an environment that is different from the current 
##  environment. Below are two functions that are used to create a 
##  special object that stores a numeric matrix and cache's its inverse.

##  The function makeCacheMatrix creates a special 
## "matrix" object that can cache the matrix inverse.
##  The object is a kind of vector or list containing functions to
##  1. set the value of the matrix
##  2. get the value of the matrix 
##  3. set the value of the matrix inverse
##  4. get the value of the matrix inverse


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setminv <- function(minv) m <<- minv
  getminv <- function() m
  list(set = set, get = get,
       setminv = setminv,
       getminv = getminv)
}

##  The function cacheSolve computes the inverse of the special 
##  "matrix" returned by makeCacheMatrix above. If the inverse has 
##  already been calculated (and the matrix has not changed), 
##  then cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
m <- x$getminv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setminv(m)
  m
}

##  Test run result
##  > m4
##  [,1] [,2] [,3]
##  [1,]    1    4    3
##  [2,]    2    1    7
##  [3,]    3   11    1
##  > m2 = makeCacheMatrix(m4)
##  > m3 = cacheSolve(m2)
##  > m3
##  [,1]        [,2]        [,3]
##  [1,] -1.3333333  0.50877193  0.43859649
##  [2,]  0.3333333 -0.14035088 -0.01754386
##  [3,]  0.3333333  0.01754386 -0.12280702
##  > m3 = cacheSolve(m2)
##  getting cached data
##  > m3
##  [,1]        [,2]        [,3]
##  [1,] -1.3333333  0.50877193  0.43859649
##  [2,]  0.3333333 -0.14035088 -0.01754386
##  [3,]  0.3333333  0.01754386 -0.12280702
