## make function list from matrix

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

cacheSolve <- function(x, ...) {
        s <- x$getinv()
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

## sample test

#for(i in 2:5){
#        m <- makeCacheMatrix(matrix(sample(1:100, i*i),i,i))
#        print(paste("i =", i))
#        print(cacheSolve(m))
#}

##test return

#[1] "i = 2"
#            [,1]         [,2]
#[1,]  0.02280130 -0.006514658
#[2,] -0.02410423  0.021172638
#[1] "i = 3"
#             [,1]        [,2]         [,3]
#[1,]  0.003675752  0.01377491 -0.011222646
#[2,] -0.029766266  0.04791911 -0.007125586
#[3,]  0.014928927 -0.02711020  0.015881448
#[1] "i = 4"
#            [,1]        [,2]         [,3]        [,4]
#[1,]  0.01384990 -0.05089410 -0.011086943  0.05592194
#[2,] -0.01223277  0.05459369  0.001559135 -0.04204354
#[3,]  0.01655627 -0.02687297  0.003197506  0.01319227
#[4,] -0.02037173  0.03818570  0.011909326 -0.02985022
#[1] "i = 5"
#             [,1]         [,2]         [,3]         [,4]         [,5]
#[1,]  0.024271813 -0.033194050  0.016895469 -0.002065197  0.005834968
#[2,]  0.006045412 -0.008469366  0.011879643  0.004411782 -0.004822551
#[3,]  0.043232080 -0.002240721 -0.041306393  0.006916031 -0.004136110
#[4,] -0.025037695  0.013349437  0.002459585  0.006046340  0.004095194
#[5,] -0.043980643  0.025623175  0.020426773 -0.012550502  0.005439616
