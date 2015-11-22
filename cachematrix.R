## These functions compute the inverse of a matrix and cache the result

## Creates a special matrix object to pass to cacheSolve function

makeCacheMatrix <- function(x = matrix()) {


	#set matrix initialised to null
	matrix <- NULL

	# matrix x set to new object y with the inverse matrix reset to null
	set <- function(y) {
		x <<- y
		matrix <<- NULL
	}

	#get matrix
	get <- function() x

	#calculates inverse matrix using solve function
	setinvmatrix<- function(solve) matrix <<-solve

	#return inverse matrix 
	getinvmatrix <- function() matrix
	
	#makeCacheMatrix function output
	list(set = set, get = get,
	setinvmatrix = setinvmatrix,
	getinvmatrix = getinvmatrix)

}


## The cachesolve function will try and retrieve the matrix inverse from the cache.
##if it fails it will calculate the inverse of the input matrix. 

cacheSolve <- function(x, ...) {

       #get cached inverse matrix 
	matrix <- x$getinvmatrix()

	#return inverse matrix from cache 
	if(!is.null(matrix)) {
		message("getting cached data")
		return(matrix)
	}

	#if no cache record exists, retrieve data to calculate inverse matrix 
	data <- x$get() 
	matrix <- solve(data, ...)
	x$setinvmatrix(matrix)
	
	#display inverse matrix output 
	matrix
}
