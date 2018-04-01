## Put comments here that give an overall description of what your
## functions do

##Returns a matrix (x) that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        #IM: Inverse Matrix
        IM <- NULL
        set <- function(y) {
                x <<- y
                IM <<- NULL
        }
        get <- function() x
        setInverse <- function(Inverse) 
                IM <<- Inverse
        getInverse <- function() IM
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        IM <- x$getInverse()
        if (!is.null(IM)) {
                message("getting cached Matrix")
                return(IM)
        }
        Mat <- x$get()
        IM <- solve(Mat, ...)
        x$setInverse(IM)
        IM
}
