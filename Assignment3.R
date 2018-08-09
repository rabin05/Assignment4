
## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function( m = matrix() ) {

	## Initialize the inverse property
    i <- NULL

    ## Method to set the matrix
    fnset <- function( matrix ) {
            m <<- matrix
            i <<- NULL
    }

    ## Method the get the matrix
    fnget <- function() {
    	## Return the matrix
    	m
    }

    ## Method to set the inverse of the matrix
    fnsetInverse <- function(inverse) {
        i <<- inverse
    }

    ## Method to get the inverse of the matrix
    fngetInverse <- function() {
        ## Return the inverse property
        i
    }

    ## Return a list of the methods
    list(fnset = fnset, fnget = fnget,
         fnsetInverse = fnsetInverse,
         fngetInverse = fngetInverse)
}


## Compute the inverse of the special matrix returned by "makeCacheMatrix" above. If the inverse has already been calculated (and the matrix has not changed), then the "cachesolve" should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {

    ## Return a matrix that is the inverse of 'x'
    m <- x$fngetInverse()

    ## Just return the inverse if its already set
    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }

    ## Get the matrix from our object
    data <- x$fnget()

    ## Calculate the inverse using matrix multiplication
    m <- solve(data) %*% data

    ## Set the inverse to the object
    x$fnsetInverse(m)

    ## Return the matrix
    m
}