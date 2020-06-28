## Creating two functions that are meant to set to and obtain inverse of a matrix
## in a normal as well as a cached scenario

## function to create and set matrix

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
        set <- function(y){
                x <<- y   
                inv <<- NULL
}
get <- function(){x}
        setInverse <- function(inverse) {inv<<- inverse}
        getInverse <- function() {inv}
        list(set = set, get = get, setInverse = setInverse,getInverse = getInverse)
  }
        

## This function enables caching the inverse of matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data") #messages about cache data
    return(inv)       #else Inverse is to be returned
  }
  mat <- x$get()
  inv <- solve(mat,...)
  x$setInverse(inv)
  inv     
        
}
