## Output for R Programming Assignment 2: Lexical Scoping
## This output aims to create functions that integrate the concepts of scoping in R for caching operations that are repeatedly executed. 

## makeCacheMatrix: This function creates a special object that can cache its inverse. The object set for this function is a matrix. 

makeCacheMatrix <- function(x = matrix()) {
  
  Red<- NULL
  set <- function (y) {
    x<<- y
    Red<<- NULL
  }
  get<- function()x
  setInverse<- function(inverse) Red<<- inverse
  getInverse<- function() Red
  list(set=set, get=get, setInverse= setInverse, getInverse= getInverse)
  
}
## cacheSolve is a function intended to retrieve the inverse of the special matrix created and cached by the function makeCacheMatrix.   

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  Red<- x$getInverse()
  if(!is.null(Red)){
    message("getting cached data")
    return (Red)
  }
  
  SpecialRed<- x$get()
  InverseRed<- solve(SpecialRed,...)
  x$setInverse(InverseRed)
  InverseRed
  
}

#Testing

#> z<- matrix(c(2,4,6,8), 2, 2)
#> z
#[,1] [,2]
#[1,]    2    6
#[2,]    4    8
#> solve(z)
#[,1]  [,2]
#[1,] -1.0  0.75
#[2,]  0.5 -0.25
#> testmatrix<- makeCacheMatrix(x= matrix(c(2,4,6,8), 2, 2))
#> cacheSolve(testmatrix)
#[,1]  [,2]
#[1,] -1.0  0.75
#[2,]  0.5 -0.25
#> testmatrix<- makeCacheMatrix(x=z)
#> cacheSolve(testmatrix)
#[,1]  [,2]
#[1,] -1.0  0.75
#[2,]  0.5 -0.25
#> testmatrix$getInverse()
#[,1]  [,2]
#[1,] -1.0  0.75
#[2,]  0.5 -0.25
#> cacheSolve(testmatrix)
#getting cached data
#[,1]  [,2]
#[1,] -1.0  0.75
#[2,]  0.5 -0.25
#> cacheSolve(testmatrix)
#getting cached data
#[,1]  [,2]
#[1,] -1.0  0.75
#[2,]  0.5 -0.25
#> cacheSolve(testmatrix)
#getting cached data
#[,1]  [,2]
#[1,] -1.0  0.75
#[2,]  0.5 -0.25

##RedPill or BluePill :) Solve(RedPill)
##end