## Matrix inversion is typically a costly computation and may take a lot of time
## and memory to run in R.

## For this reason, caching the inverse of a matrix may prove to be beneficial
## rather than having R compute it repeatedly using a loop. Once the inverse of a
## matrix has been computed once, it can be looked up in the cache rather than
## be re-computed using the loop function. The following two functions
## can be used to cache the inverse of a matrix.


## The first function, makeCacheMatrix, creates a special "matrix",
## which is really a list containing a function to

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix



makeCacheMatrix <- function(x = matrix()) {
    #'inv' will store the cache inverse matrix
    inv <- NULL
    
    #set the value of the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    #get the value of the matrix
    get <- function() x
    
    #set the value of the inverse matrix
    setinverse <- function(inverse) inv <<- inverse
    
    #get the value of the inverse matrix
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



## The second function, cacheSolve, returns the inverse of the special
## "matrix" created with the above function.

## It will first check to see if the inverse of the matrix has already
## been calculated. If so, it displays the result from the cache and
## skips the computation. If it has not been previous calculated,
## it will not calculate the inverse and sets the value of the inverse
## in the cache via the 'setinverse' function.

## This function assumes that the matrix is always inversible


cacheSolve <- function(x, ...) {
    #get cache value for inverse
    inv <- x$getinverse()

    #if cache is not empty, return inverse
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }

    # The inverse is not yet calculated. We need to calculate it, cache it & return it
    data <- x$get()         #get value of matrix
    inv <- solve(data)      #calculate inverse matrix
    x$setinverse(inv)       #cache inverse matrix
    inv                     #return inverse
}



## Example creating matrix x and caching inverse

# > x <- matrix(rnorm(25), nrow = 5)        #create matrix x
# > specialx <- makeCacheMatrix(x)          #create special matrix
# > specialx$get()                          #return special matrix
# > cacheSolve(specialx)                    #return the inverse of special matrix
# > cacheSolve(specialx)                    #inverse already calculated, return cached inverse

