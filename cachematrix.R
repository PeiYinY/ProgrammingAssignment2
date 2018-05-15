## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) { ## define the argument with default mode of "matrix"
    inv <- NULL   ## create a empty space which allows to hold the values of inverse matrix
    
    set <- function(y){  ## define a set function to assign new values of matrix
        x <<- y          ## in parent environment.
        inv <- NULL  ## if there is a new matrix, reset inv to NULL
    }
    get <- function() x  ## define a get function which returns the values of the matrix argument
    setInverse <- function(inverse) inv <<- inverse ## assign value of inv in parent environment
    getInverse <- function() inv ## gets the value of inv 
    list(set = set, get = get,  ## in order to refer to the function with the $ operator
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
        }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
