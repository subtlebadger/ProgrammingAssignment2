## The following two functions are used to cache the inverse of a matrix

## makeCacheMatrix creates a list containing a function to:
## a. set the value of the matrix
## b. get the value of the matrix
## c. set the value of inverse of the matrix
## d. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        i = NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() inv
        list(set = set, 
             get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}

## cacheSolve returns the inverse of the matrix. It first checks whether the
## inverse has already been computed. If so, it gets the result and skips the
## computation. If not, it computes the inverse and sets the value in the cache 
## via the setinverse function.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i) 
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
}