## Put comments here that give an overall description of what your
## functions do
## The functions below take a matrix and create a special matrix out of it that calculates the inverse and caches it. 
## If the inverse is already calculated, the cached value will be returend. 
## Otherwise, the inverse will be computed and stored in the special matrix

## Write a short comment describing this function
## This function created a special matrix with helper functions out of a normal matrix

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
        	x <<- y
        	i <<- NULL
    	}
    	get <- function() x
    	setinverse <- function(inverse) i <<- inverse
    	getinverse <- function() i
    	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## This function checks if the inverse of a matrix is cached, if yes, it retrieves it, if no, it calculates the inverse and caches it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	i <- x$getinverse()
    	if(!is.null(i)) {
       		message("getting cached data")
        	return(i)
    	}
    	data <- x$get()
    	i <- solve(data)
    	x$setinverse(i)
    	i	
}
