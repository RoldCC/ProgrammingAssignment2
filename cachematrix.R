## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(g){
                if (nrow(g)!=ncol(g)){
                        return("The matrix need to be squared")
                }
                else{
                        x <<- g
                        inv <<- NULL
                }
        }
        get <- function() x
        setinv <- function(m) inv <<- m
        getinv <- function() inv
        list(set = set,
             get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_mat <- x$getinv()
        if (!is.null(inv_mat)){
                message("obtaining the inverse...")
                return(inv_mat)
        }
        else {
                info <- x$get()
                calc_inv <- solve(info)
                x$setinv(calc_inv)
                calc_inv
        }
}
