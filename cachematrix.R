## makeCacheMatrix:
## To facilitate this caching, you first create a special
## matrix that will help us with this by using the
## makeCacheMatrix function.  The input into this function
## is simply a variable of type matrix.

## makeCacheMatrix: return a list of functions to:
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse
## 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    # Initialize invr variable
    invr <- NULL
    
    # Set function
    # Create a function that is to keep x and invr variables 
    # as passed matrix and Null
    set <- function(y) {
        x <<- y
        invr <<- NULL
    }
    
    # Get function
    # Gets the matrix itself but not the inverse
    get <- function() x
    
    # Manually set the inverse
    setinverse <- function(inverse) invr <<- inverse
    
    # Get the inverse
    getinverse <- function() invr
    
    list(set=set, 
         get=get, 
         setinverse=setinverse, 
         getinverse=getinverse)
    
}


## cacheSolve:
## This function computes the inverse of matrix.
## by checking previous history, this function avoids for redundancy.

cacheSolve <- function(x, ...) {
    # Return a matrix that is the inverse of 'x'
    invr <- x$getinverse()
    
    if(!is.null(invr)) {
        # Check the result.
        # by checking if invr is NULL, we can know whether this matrix was already computed or not.
        # if so, return computed value in last time, then print the message.
        message("getting cached data.")
        return(invr)
    }
    
    # if invr is NULL, the inverse of matrix is computed by solve() function.

    data <- x$get()
    invr <- solve(data)
    
    # Cache this result in the object
    x$setinverse(invr)
    
    # Return this result
    invr
}

