## Make a special version of matrix that keeps a cache
## version of the inverse for the matrix. This improves 
## the performance.
## usage:
##  m<- matrix(c(1,2,3,4), nrow=2, ncol=2)
##  > cm <- makeCacheMatrix(m)
##  > cacheSolve(cm)

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function computes the inverse of a matrix x.
## It returns a cached version if it is available.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached inverse of the matrix")
                return(i)
        }
        else {
          ## call the actual solve function to get inverse.
          data <- x$get()
          i <- solve(data, ...)
          ## cache the inverse of the matrix
          x$setinv(i)
          return(i)
       }
}
