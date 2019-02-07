## These two functions combined will create a matrix, calculate and cache the 
## inverse of the matrix

## makeCacheMatrix will create a matrix and cache its inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  
  I <- NULL
  set <- function(y) {
    x <<- y
    I <<- NULL
  }
  get <- function() x
  setInverse <- function(Inverse) m <<- Inverse
  getInverse <- function() I
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## cacheSolve will call the inverse matrix. If inverse matrix does not exist,
## it will caculate the inverse matrix

cacheSolve <- function(x, ...) {
    I <- x$getInverse()
    if(!is.null(I)) {
      message("getting cached inverse matrix")
      return(I)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(I)
    I
}
