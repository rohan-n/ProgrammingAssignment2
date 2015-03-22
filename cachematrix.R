## My functions facilitate the process of finding the inverses of matrices.

## This function creates 4 member functions: set, get, setInverse, getInverse.

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL

    set <- function(y) {

        x <<- y

        inv <<- NULL

    }

    get <- function() x

    setinverse <- function(inverse) inv <<- inverse

    getinverse <- function() inv

    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function solves the matrices and gets the inverse, if it has not already been found. It caches the result. 

cacheSolve <- function(x, ...) {
       cacheSolve <- function(x, ...) {

    inv <- x$getinverse()

    if(!is.null(inv)) {

        message("getting cached data.")

        return(inv)

    }

    data <- x$get()

    inv <- solve(data)

    x$setinverse(inv)

    inv

}
