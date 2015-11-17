## Because a matrix inversion is a costly computation I write a function that cache the inverse of a matrix to avoid computing it repeatedly.

## I first create a list containning a function that set the value of the matrix, get the value of the matrix, set the value of inverse of the matrix, get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y){
       x<<-y
       inv<<-NULL
     }
     get <- function()x
     setinverse <- function(inverse) inv<<- inverse
     getinverse <- function()inv
     list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## I create a function that returns the inverse if the matrix. If the inverse has been computed the function retrieve the inverse from the cache. If the inverse has not been computed the function should compute the inverse, sets the value in the cache using the setinverse function.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)){
          message("getting cache data")
          return(inv)
        }
        data <-x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
