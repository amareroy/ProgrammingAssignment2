## This function creates a special "matrix" object that can cache its inverse.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        invrs <- NULL
        set <- function(a) {
                x <<- a
                invrs <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) invrs <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# The following function returns the inverse of the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invrs <- x$getinverse()
        if(!is.null(invrs {
                message("getting cached data.")
                return(invrs)
        }
        data <- x$get()
        invrs <- solve(data)
        x$setinverse(invrs)
        invrs        
}
