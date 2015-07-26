##################################################################################################
## The two functions below create a process for making  an "object" (it is actually 		##
## an R environment) that contains a matrix, its inverse, and a set of functions that		## 
## permit the user to interact with the object to get and set the matrix, and get and set 	##
## the matrix's inverse.									##
##################################################################################################


## makeCacheMatrix is a closure.  It takes a matrix as an argument. 
## It returns a list of functions, but also remains resident
## in memory 

makeCacheMatrix <- function(x = matrix()) {
	matrixInverse <- NULL 			
	setMatrix <- function(y) {			
		x <<- y  			
		matrixInverse <<- NULL  	
	}
	getMatrix <- function() x  		
	setInverse <- function(inverse) matrixInverse <<- inverse  
	getInverse <- function() matrixInverse 		
	return(list(setMatrix = setMatrix, 
		    getMatrix = getMatrix,    
		    setInverse = setInverse,
		    getInverse = getInverse))
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	i <- x$getInverse()
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}	
	data <- x$getMatrix()
	i <- solve(data, ...)
	x$setInverse(i)
	i
}
