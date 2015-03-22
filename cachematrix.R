## The two functions below create a new type of object that is similar
## to a matrix, but which also has the ability to store the inverse of the
## matrix in a "cache". This is useful in a situation where the inverse of the 
## matrix needs to be used repetedly, because it allows the inverse to be
## computed once, then stored in a cache from which it can be retrived,
## rather than needing to be re-computed each time.


## The makeCacheMatrix function below creates the special "matrix"-like
## object that can cache its inverse. The idea is to create an object similar
## to a matrix, but which can store its inverse so that the inverse can
## simply be pulled from memory rather than needing to be recomputed each time
## the matrix is used.

makeCacheMatrix <- function(x = matrix()) {
    mat <- NULL
    set <- function(y){
        x <- y
        mat <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) mat <<- inverse
    getinv <- function() mat
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## The cacheSolve function below computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been
## calculated, the function will retrieve the inverse from the cache rather
## than recomputing it.

cacheSolve <- function(x, ...) {
    mat <- x$getinv()
    if(!is.null(mat)){
        message("getting cached data")
        return(mat)
    }
    data <- x$get()
    mat <- solve(data, ...)
    x$setinv(mat)
    mat
}
