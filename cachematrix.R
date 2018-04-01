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


cacheSolve <- function(x, ...) {
        IM <- x$getInverse()
        if(!is.null(IM)) {
                message("getting cached Matrix")
                return(IM)
        }
        Mat <- x$get()
        IM <- solve(Mat, ...)
        x$setInverse(IM)
        IM
}
