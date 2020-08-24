## This first function takas a squared matrix(if it is not squared send a error message 
## saying that you need to inpunt a squared one and stop the function) and save it into cache, also if youn want you can
## store a inverse version of this matrix and store it there, at the end return a list containing what your input
## was.

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


## This second function takes the output of the fuction above and see if there is an inverse store into cache,
## if it is true, retunrs it, if not, calculate and return it.

cacheSolve <- function(x, ...) {
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
