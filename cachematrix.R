## In this assignment we are using special operator(<<-) to manage variables
## at different environments(parent and child).  In the parent environment the
## special operator helps to assign the value(NULL to inverse of matrix, and 
## y to matrix x), get the matrix x, and to set and get the value of inverse 
## of the matrix x, then store them in the cache. So, the function in child 
## environment gets the cache and skips the cmputation.  It makes the 
## computation much more efficiency.

## The first function below, makeCacheMatrix, is a list containing function 
## with the help of special operator to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of matrix
## 4. get the value of inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<- function(y) {
    x<<- y
    m<<- NULL
      }
  get<- function() x
  setInverse<- function(solve) m<<- solve
  getInverse<- function() m
  list(set=set, get=get,
       setInverse=setInverse,
       getInverse=getInverse)

}


## The function below, cacheSolve, calculates the inverse of the matrix from
## the first function.  However, it first checks to see if the inverse has 
## been calculated.  If so, it gets the inverse from cache and skips the 
## computation.  Otherwise, it calculates the inverse of the matrix
## and sets the value of the inverse in the caches the setInverse function.
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x=matrix(), ...) {
       
  m<- x$getInverse()
  if(!is.null(m)) {
    message ("getting cached data")
    return(m)
  }
  matrix<- x$get()
  m<-solve(matrix, ...)
  x$setInverse(m)
  m
}
z<- makeCacheMatrix()
z$set(matrix(runif(25),5,5))
cacheSolve(z)
