## Matrix inversion is usually a costly computation 
##and there may be some benefit to caching the inverse of a matrix rather
##than compute it repeatedly 

##Here we write following two functions that cache the inverse of a matrix.
##The first function  makeCacheMatrix creates a special "matrix",which is really a list containing a function to.

# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix



makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The following function calculates the inverse of the special "matrix" created with the makeCacheMatrix function.
##However, it first checks to see if the inverse has already been calculated. 
## If so,it gets the inverse from the cache and skips the computation.
##Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.


# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}




##Sample Output
##x=rbind(c(2,3),c(2,2)) 
## m=makeCacheMatrix(x)

## m$get()
##[,1]  [,2]
##[1,]  1.00 -0.25
##[2,] -0.25  1.00

## cacheSolve(m)       - running first time  calculates the inverse and set the cache
##[,1] [,2]
##[1,]   -1  1.5
##[2,]    1 -1.0

## cacheSolve(m)      -since the matrix has not changed, this time cached data is returned
##getting cached data.
##[,1] [,2]
##[1,]   -1  1.5
##[2,]    1 -1.0

