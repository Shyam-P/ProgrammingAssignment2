## makeCacheMatrix() provides set of methods for writing & reading matrix data
## cacheSolve() is the method to get the matrix inverse using the special list 
## & methods created by makeCacheMatrix(). This approach allows for the inverse
## to be cached & fetched quickly, if the source matrix data has not changed 
## since last solve. If source has changed, it  is solved and cached.

## Typical usage: 
## First create 'special matrix element' using a <- makeCacheMatrix() 
## Then, use a$set(test_matrix) to set the matrix data, & a$get() to retrieve
## Use cacheSolve(a) to get the inverse of matrix data in 'a'

##
## makeCacheMatrix() creates a special list with methods that allow
## reading and writing the matrix data & its inverse. Using these methods
## allows tracking any changes to the source matrix data.
##
## The set() & get() methods, write and read the matrix data; while 
## the setinv() & getinv() methods write and read the matrix inverse data.
## $setinv(matrix_data) sets/stores the inverse matrix data, and
## $getinv() returns the cached inverse matrix data as a part of a list
##
## A boolean variable 'm' tracks changes to the source data 'x'
## This is set to FALSE when the source matrix data is set, and 
## this is set to TRUE when the matrix is solved & the inverse cached in 'xinv'
##
## NOTE: methods in this DO NOT SOLVE for the inverse, only store data
makeCacheMatrix <- function(x = matrix()) {
	m <- FALSE
	xinv = matrix()
	
	set <- function(y) {
			x <<- y
			m <<- FALSE			# Set flag to prevent use of cached xinv
			xinv <<- matrix() 	# Reset xinv as data has changed
	}
	get <- function() x
	
	setinv <- function(y) {
		m <<- TRUE
		xinv <<- y
	}
	getinv <- function() list(m, xinv)
	
	list(set = set, get = get,
		 setinv  = setinv,
		 getinv = getinv)
}


## Write a short comment describing this function
## cacheSolve uses the methods in makeCacheMatrix() to cache the inverse matrix
## First, getinv() is called which returns the cached inverse data and a flag 
## indicating whether that is current with the source data. If flag is TRUE,
## cached matrix inverse is returned.
##
## If flag is FALSE, the matrix data is fetched, solved and cached in 
## makeCacheMatrix and the flag is updated to TRUE.
##
## NOTE: No error checking is done in this version as it is assumed that the
##       matrix is invertible. Else, error messages need to be cached too
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		list1 <- x$getinv()
		
		# Check if cached value can be used
		if ( list1[[1]] ) {
			message("getting cached data")
			return(list1[[2]])
		} else {
			# Need to solve for inverse as matrix data has changed
			data = x$get()
			xinv = solve(data)
			x$setinv(xinv)
			xinv
		}
}
