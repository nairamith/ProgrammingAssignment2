## The functions below can be used to calculate the  inverse of a 
## matrix. The functions use caching mechanism to avoid the 
## costly computation involved with getting the inverse of a matrix
## repeatedly

## The function takes a matrix input and returns a 
## matrix of funcions to
## 1. Set the matrix
## 2. Set and cache the inverse of the matrix
## 3. Get the matrix
## 4. Get the inverse of the matrix if available in the cache

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(y) {
    x <<- y
    i <<- NULL
  }
  get<-function()  x
  
  setInverse<-function(inverse) {
    i<<-inverse
  }
  getInverse<-function() i
  
  matrix(c(set, setInverse, 
           get, getInverse), 2,2, byrow = TRUE,
         dimnames = list(c("set", "get"),c("Matrix","Inverse")) )

}


## This method takes the return value of the function
## makCacheMatrix and returns the inverse of the matrixbound to
## the passed function

cacheSolve <- function(x, ...) {
  inverse<-x["get", "Inverse"][[1]]()
  if(!is.null(inverse)){
    message("Reading Fromm Cache")
    return(inverse)
  }
  matrix<-x["get", "Matrix"][[1]]()
  inverse<-solve(matrix)
  x["set", "Inverse"][[1]](inverse)
  inverse
}
