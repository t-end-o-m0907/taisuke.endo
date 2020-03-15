## Put comments here that give an overall description of what your
## functions do

## make function list from matrix
makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) s <<- solve
        getinv <- function() s
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## calculate inverse matrix using makeCashMatrix
cacheSolve <- function(x, ...) {
        s <- x$getinv
        if(!is.null(s)) {
                message("getting cached data!")
                return(s)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(s)
        s
}
