## Functions below are used together to improve efficiency, through caching,
##   of matrix inversion


## Function: makeCacheMatrix
## Description: creates a function containing 4 child functions
##			set, get, setsolve, getsolve
## Inputs: matrix - optional
## Returns: list of available child functions

makeCacheMatrix <- function(x = matrix()) {

	# create a new vector to reference 
	m <- NULL

	# create the anonymous set function for the parent function
	set <- function(y) {
		x <<- y
		m <<- NULL
	}

	# assigns function 'x' to 'get'
	get <- function() x

	# assigns reference of vector 'solve' to m, then to 'setsolve'
	setsolve <- function(solve) m <<- solve

	# assigns function 'm' to getsolve
	getsolve <- function() m

	# return list of available functions created above
	list(set = set, get = get,
		setsolve = setsolve,
		getsolve = getsolve)	

}


## Function: cacheSolve
## Description: returns inverse of 'x' matrix created by 'makeCacheMatrix'
## Inputs: matrix created by the 'makeCacheMatrix' function
## Returns: a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
	# assigns the value of getsolve in 'x' to 'm'
	m <- x$getsolve()        

	# if 'm' is not null, meaning 'x' already ran getsolve function, then
	# return message and value of m
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}

	# get matrix
	data <- x$get()
	
	# solve matrix as assigned to 'm'
	m <- solve(data, ...)
	
	# assign solved 'm' back to original matrix
	x$setsolve(m)
	
	# return inversed matrix
	m
}
