##################################################################################################
## The two functions below create a process for making  an "object" (it is actually 		##
## an R environment) that contains a matrix, its inverse, and a set of functions that		## 
## permit the user to interact with the object to get and set the matrix, and get and set 	##
## the matrix's inverse.									##
##################################################################################################


## makeCacheMatrix (below) is a closure.  It takes a matrix as an argument. 
## It returns a list of functions, but also, as a closure, remains resident
## in memory after it executes, so that its functions can access the propoerties of the matrix 
## originally passed to it.  makeCacheMatrix's subordinate functions can also access the values of 
## variables (like, in this case, matrixInverse) that are free variables in the subordinate 
## functions (i.e. the child environment) but local variables within makeCacheMatrix itself (i.e. 
## the parent environment).

makeCacheMatrix <- function(x = matrix()) {
	# When makeCacheMatrix is first called, it sets the inverse to null.  Subsequent calls to 
	# setInverse can set the inverse of the matrix.  Note that the inversion does not happen
	# within any of the functions inside makeCacheMatrix.  
	matrixInverse <- NULL 			
	setMatrix <- function(y) {			
		x <<- y  			
		matrixInverse <<- NULL  	
	}
	getMatrix <- function() x  		
	setInverse <- function(inverse) matrixInverse <<- inverse  
	getInverse <- function() matrixInverse
	# The first entry in each list element below is the name of the list element, the second is its
	# value, which in this case is referring to one of the functions defined within makeCacheMatrix.
	return(list(setMatrix = setMatrix, 
		    getMatrix = getMatrix,    
		    setInverse = setInverse,
		    getInverse = getInverse))
}


## cacheSolve takes as input a "matrix" (actually an environment) created with makeCacheMatrix.
## If the inverse of the matrix created with makeCacheMatrix has already been stored in that environment,
## cacheSolve returns the inverse.  If the inverse hasn't been stored there, cacheSolve retrieves the 
## matrix stored in that environment, calculates its inverse, stores the inverse in the environment created
## with makeCacheMatrix, and returns the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	i <- x$getInverse()				# Get the matrix inverse stored in the X environment.
	if(!is.null(i)) {				# If it's not null, then return it.
		message("getting cached data")
		return(i)
	}	
	data <- x$getMatrix()				# Else, get the matrix stored in the X environment
	i <- solve(data, ...)				# Calculate its inverse
	x$setInverse(i)					# Store the inverse in the X environment
	i   						# Return the inverse
}
