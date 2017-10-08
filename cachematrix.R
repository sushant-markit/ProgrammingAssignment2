## makeCacheMatrix function computes a special list containing setter
## and getter functions for saving and getting a matrix
## and saving and getting its inverted matrix, which is cached.

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}
	get <- function() x
	setInverse <- function(inv) inverse <<- inv
	getInverse <- function() inverse

	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve function fetches the inverted matrix from the cache if available
## else it computes the inverse of the matrix afresh, saves it in the cache
## and prints it on the console.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()

    ## if inverse of the matrix is cached then use it else compute it.
    if(!is.null(inverse)) {
    	message("getting cached inverted matrix")
    	return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
    inverse
}
