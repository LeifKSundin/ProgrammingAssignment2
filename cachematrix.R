## I made the first makeCacheMatrix function so that it creates a list of 4 functions, 
## setMatrix and getMatrix are what the sound like, you set and get your matrixes.
## The following two set and gets your inverse of that matrix. z have been assigned to cache the computation.
## cacheSolve checks with an if() statement if there are a cache, 
##if not it computes it and stores it in the makeCacheMatrix() with the setInv functions.
## I have never programmed before so I hope i got it right

## function that stores its own inverse

makeCacheMatrix <- function(x = matrix()) {
	z <- NULL 						
	setMatrix <- function(y = matrix()) {
		x <<- y
		z <<- NULL
	} 							
	getMatrix <- function() x 			
	setInv <- function(Inverse) 	
		z <<- Inverse
	getInv <- function() z				
	list(setMatrix = setMatrix, getMatrix = getMatrix, 
		setInv = setInv, getInv = getInv)
}

## Stores and/or just computes the inverse of the makeCacheMatrix()




cacheSolve <- function(x, ...) {
	z <- x$getInv()
	if(!is.null(z)) {
		message("getting cached data")
		return(z)
	}				
	mtr <- x$getMatrix()	
	z <- solve(mtr,...)
	x$setInv(z)
	z
}

