## makeCacheMatrix gets and sets a matrix and its inverse
## cacheSolve computes the inverse of a matrix if it is not already cached by makeCacheMatrix
## EXAMPLE: cacheSolve computes a matrix inverse
## > mat = rbind(c(1, -1/4),c(-1/4,1))
## > test <- makeCacheMatrix(mat)
## > test2 <- cacheSolve(test)
## > test$getinv()
## [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
## > test$get()
## [,1]  [,2]
## [1,]  1.00 -0.25
## [2,] -0.25  1.00

## cache of a matrix and its inverse
## both the matrix and its inverse can be get
## and set with this function

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y){
        x <<- y
        inv <<- NULL
      }
      get <- function() {
        x
      }
      setinv <- function(invMat) {
        inv <<- invMat
      }
      getinv <- function() {
        inv
      }
      list(set=set, get=get,
           setinv = setinv,
           getinv = getinv)
}


## computes the inverse of a square matrix if the 
## inverse is not already cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getinv()
      if(!is.null(inv)){
        message("getting cached data")
        return(inv)
      }
      datainv <- x$get()
      inv <- solve(datainv,...)
      x$setinv(inv)
      inv
}
