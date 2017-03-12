## This function makes a matrix to be use as an input of the cacheSolve function
## 


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This fuctions calculates the inverse of the matrix created by makeCacheMatrix
## if the inverse already exist it give the cahed value 
 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        if(!class(try(solve(data,silent=T)))=="matrix"){
                message("matrix is singular")
                return()
        }
        m <- solve(data, ...)
        x$setinv(m)
        m
}
