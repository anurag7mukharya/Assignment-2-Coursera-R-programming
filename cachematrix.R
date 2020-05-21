##Lexical Scoping
##This is a function makeCacheMatrix created to set the values of the matrix
## get its value and set the inverse of the matrix and get the inverse of the 
## matrix. Matrix can cache its object
##The <<- operator which can be used to assign a value to an object in an 
##environment that is different from the current environment

makeCacheMatrix <- function(x = matrix()) {  #create function which creates a special matrix that can cache its inverse
  invm <- NULL    
  setMatrix <- function(y) {  #matrix value is being set in this step
    x <<- y  
    invm <<- NULL
  }
  getMatrix <- function() x     #get the value of the matrix
  setInverse <- function(solveMatrix) invm <<- solveMatrix   #set the value of the inverse of the matrix, also note the use of Solve function
  getInverse <- function() invm   #get the value of the inverse of the matrix
  list(setMatrix = setMatrix, getMatrix = getMatrix,  
       setInverse = setInverse,
       getInverse = getInverse)
}




## CacheSolve Function takes the output created by the function makeCacheMatrix() 
##runs it as an input and do the inverse of the matrix after checking if the function 
## makeCache Matrix() has some values stored in it
## so basically it performs two steps:
## 1. checks if inverse matrix is empty, if yes, it gets the matrix data and set the inverse
## matrix by using the solve() function
##2. checks if inverse matrix has some value then it gives the message - "getting cached Inverse Matrix data"
## along with the cached data

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  invm <- x$getInverse()
  if(!is.null(invm)) {    #check if inverse matrix is not Null
    message("getting cached Inverse Matrix data")  # type the message based on the previous step
    return(invm)  #return the invertible matrix
  }
  data <- x$getMatrix()     #get original matrix data
  invm <- solve(data, ...)   #use solve function to inverse the matrix, inverse function can also be used instead of solve()
  x$setInverse(invm)   # set the invertible matrix
  invm
}

