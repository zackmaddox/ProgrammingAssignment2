## These functions represent a matrix object that stores it's inverse in a cacheable manner.

## This function creates the matrix object described above.  It stores the matrix and its inverse.  Getter/setter functions are provided for 
## getting/setting the values of the matrix and it's inverse.  

## THE cacheSolve FUNCTION BELOW SHOULD BE USED FOR RETRIEVING THE VALUE OF THE MATRIX"S INVERSE TO TAKE ADVANTAGE OF CACHING.    

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function is used for retrieving the inverse of the matrix of the object described by the makeCacheMatrix function.
## THIS FUNCTION SHOULD ALWAYS BE USED IN ORDER TO TAKE ADVANTAGE OF THE CACHING.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if (!is.null(i)) {
        	message("Getting cached data")
        	return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
}
