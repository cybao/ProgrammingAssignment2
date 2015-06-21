## This file contains two functions: makeCacheMatrix and cacheSolve. Together these functions output the 
##inverse of a square matrix either by computing the inverse directly or by reading from cached values


## The 'makeCacheMatrix' creates a special "matrix" which is a list containing 4 functions:
##  'set': set the value of the matrix
##  'get': set the value of the matrix
##  'setinverse': set the inverse of the matrix
##  'getinverse': get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

		m <- NULL ## m is the inverse matrix, set to NULL to begin with

		## 'set' function sets the value of the matrix, inverse still not calculated		
		set <- function(y) {
		       x <<- y
		       m <<- NULL
		}

		## 'get' function gets the value of the matrix from cache
		get <- function() x

		## 'setinverse' function sets the inverse of the matrix
		setinverse <- function(inverse) m <<- inverse

		## 'getinverse' function gets the inverse of the matrix from cache
		getinverse <- function() m

		## put all functions into a list
		list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)

}


## The 'cacheSolve' function calculates the inverse of the special "matrix" created with
## the 'makeCacheMatrix' function. If the inverse of a matrix has been calculated, the
## cached inverse is read in. Otherwise the inverse of the matrix is computed and cached.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'	
	m <- x$getinverse()

	## if the inverse is not NULL, which means the inverse has been calculated,
	##  then return the cached inverse
	if(!is.null(m)) {
			message("getting cached data")
			return(m)
	}
	
	## if the inverse has not been calculated
	## first get and put the matrix into a variable
	data <- x$get()

	## then compute the inverse of the matrix
	m <- solve(data,...)

	## store the inverse into cache
	x$setinverse(m)

	## return the inverse
	m
}
