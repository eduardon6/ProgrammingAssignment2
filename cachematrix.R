## The functions calculate the inverse of a matrix

## makeCacheMatrix takes a square matrix as an input and creates a list with 4 different functions: 
## first element) changes the matrix in the function environment, second element) sets the inverse of 
## the matrix, third element) gets the matrix, fourth element) gets the inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {
      
      inverse <- NULL
      setmatrix <- function(y) {
            if(ncol(y) != nrow(y)) {
                  message("CANNOT ACCEPT A RECTANGULAR MATRIX ")
            }
            else {
                  x <<- y 
            } 
      }
      setinverse <- function(y) inverse <<- y
      getmatrix <- function() x
      getinverse <- function() inverse
      
      if(ncol(x) != nrow(x)) {
            message("CANNOT ACCEPT A RECTANGULAR MATRIX ")
            }
      else {
            list <- c(setmatrix = setmatrix,
                      setinverse = setinverse, 
                      getmatrix = getmatrix, 
                      getinverse = getinverse)
      }
}



## cacheSolve gets the inverse of the matrix if it's already specified in makeCacheMatrix. 
## If the inverse is not already specified, the function calculates the ivnerse and stores 
## it in the environment of makeCacheMatrix

cacheSolve <- function(x, ...) {
      inverse <- x$getinverse()
      if(!is.null(inverse)){
            message("getting chached data")
      } else{
            message("getting inverse...")
            matrix <- x$getmatrix()
            inverse <- solve(matrix)
            x$setinverse(inverse) 
      }
      return(inverse)
}


