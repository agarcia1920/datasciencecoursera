## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# The function below uses the matrix in the argument to create "smaller functions" within the main function
# that stores the matrix based on the function argument, can call the matrix, 
# sets the inverse of the original matrix, and gets the values of the inverted matrix.

makeCacheMatrix <- function(x = matrix()) {
  mtrx <- NULL
  set <- function(y) {
    x<<- y
    mtrx <<- NULL
  }
  get<-function() x
  setinverse<-function(inverse) mtrx<<-inverse
  getinverse<-function() mtrx
  list(set=set,get=get,setinverse=setinverse, getinverse=getinverse)
  
}



## Write a short comment describing this function

# The function below first checks if the matrix has been inversed and cached in the function's argument.
# If it has, then that cached data is returb1ned. If not, the inverse is gathered using "get" on the argument and assigning it to data.
# Then is used on data "solved" for and we can gather the inverse and set it to "invse," which will be returned to the user.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invse<-x$getinverse()
  if (!is.null(invse)) {
    message("getting cached data")
    return(invse)
    
  }
  data<-x$get()
  invse<-solve(data, ...)
  x$setinverse(invse)
  invse
}
