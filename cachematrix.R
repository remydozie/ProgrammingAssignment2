## makeCacheMatrix creates a special matrix object, and thereafter cacheSolve
## calculates the inverse of the matrix.
## If the matrix inverse has already been calculated, cacheSolve will instead
## find it in the cache and return it, and not calculate it again.

makeCacheMatrix <- function(x = matrix()) {
    inv_x <- NULL
    set <- function(y) {
        x <<- y
        inv_x <<- NULL
    }
    get <- function() x
    setinverse<- function(inverse) inv_x <<-inverse
    getinverse <- function() inv_x
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## The function cacheSolve returns the inverse of a matrix A created with
## the makeCacheMatrix function as above.
## If the cached inverse is available, cacheSolve retrieves it, otherwise,
## it computes, caches, and returns it.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv_x <- x$getinverse()
    if (!is.null(inv_x)) {
        message("getting cached inverse matrix")
        return(inv_x)
    } else {
        inv_x <- solve(x$get())
        x$setinverse(inv_x)
        return(inv_x)
    }
}
# Testing functions
> z <- diag(2, 3)
> z
     [,1] [,2] [,3]
[1,]    2    0    0
[2,]    0    2    0
[3,]    0    0    2
> new_matrix <- makeCacheMatrix(z)
> cacheSolve(new_matrix)
     [,1] [,2] [,3]
[1,]  0.5  0.0  0.0
[2,]  0.0  0.5  0.0
[3,]  0.0  0.0  0.5
> cacheSolve(new_matrix)
getting cached inverse matrix
     [,1] [,2] [,3]
[1,]  0.5  0.0  0.0
[2,]  0.0  0.5  0.0
[3,]  0.0  0.0  0.5
