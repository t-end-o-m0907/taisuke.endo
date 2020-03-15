## make function list from matrix

###library(MASS)

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setinv <- function(solve) s <<- solve
        getinv <- function() s
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## calculate inverse matrix using makeCashMatrix
### if the diemnsion of matrix is higher than 2, use ginv().

cacheSolve <- function(x, ...) {
        s <- x$getinv
        if(!is.null(s)) {
                message("getting cached data!")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        #s <- ginv(data, ...)
        x$setinv(s)
        s
}
