## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix keeps a Cache of the matrix and returns the same when called upon

makeCacheMatrix <- function(x = matrix()) {
          m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set=set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
## cacheSolve calls the makeCacheMatrix to see if there is an inverse value already in cache. 
## If there is not, it computes the inverse and also calls makeCacheMatrix to store the result in cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
}
