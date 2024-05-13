## David Willard, Week 3 Programming Assignment

## Function makeCacheMatrix creates a "matrix" with the abilty to set the 
## matrix, get the matrix, solve for the value of the matrix and save it, and
## get the already saved value of the inverse.

## Test data I used for the below functions was invertible matrix:
## A <- matrix(c(1,-4,2,-2,1,3,2,6,8),nrow = 3,ncol = 3, byrow = TRUE)

## Results in:
##             [,1]        [,2]       [,3]
## [1,]  0.07936508 -0.34920635 0.11111111
## [2,] -0.17460317 -0.03174603 0.05555556
## [3,]  0.11111111  0.11111111 0.05555556

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    
    ## Method to set the matrix.
    set <- function(y) {
      x <<- y
      i <<- NULL
    }

    ## Return the stored matrix.
    get <- function() x
    
    ## Cache an inverted matrix.
    setInverse <- function(solve) i <<- solve
    
    ## Return the inverted matrix.
    getInverse <- function() i
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Function cacheSolve returns a matrix the in the mathematical inverse of a
## matrix that was created using the makeCacheMatrix function above.  It will
## first check if there is already a computed inverse in cache.  If there is, 
## it returns the inverse from cache. If not, it computes the inverse and
## sets the values.

cacheSolve <- function(x, ...) {

    i <- x$getInverse() ## Set i to the cached inverse, will be NULL if not
                        ## already computed.
    
    if(!is.null(i)) { ## Not NULL if inverse is already in cache. 
      message("getting cached data")
      return(i)  ## Return the cached inverse.
    }
    
    ## i was null so now it will compute the inverse, cache it, and return
    ## the inverse.
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}

