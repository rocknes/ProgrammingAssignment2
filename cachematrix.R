## We are trying to cache an expensive matrix operation viz. inversion of matrix.
## We will be given a square matrix, and we are supposed to return the inverse of it.
## we need to make sure that matrices which has already been inverted,
## should not be inverted again, instead we should return the previously computed result.
## To achieve this, we will use two functions 1. makeCacheMatrix 2. cacheSolve
## The functions are described below individually


## Function makeCacheMatrix() can be viewed as a container which holds the user provided matrix, 
## it's invert and few functions to retrieve and set both the quantities.
## makeCacheMatrix function take a square matrix as input (assumption as per the assignment statement),
## and returns a list which has the 4 functions.
## im is a variable local to the makeCacheMatrix Frame, it stores the result and is initialized to NULL
## set is function that takes a matrix and over-writes the formal argument variable x
## get is a function that returns the user provided matrix
## setSolution is a function which stores a given inversion result
## getSolution is a function that returns the cached solution

makeCacheMatrix <- function(x = matrix()) {
	im <- NULL
	set <- function(y) {
		x <<- y
		im <<- NULL
	}
	get <- function() x
	setSolution <- function(solution) im <<- solution
	getSolution <- function() im
	list(set = set, get = get,
			setSolution = setSolution,
			getSolution = getSolution)
}


## cacheSolve is the function which takes the list object returned by makeCacheMatrix as input and gives the inverse of that matrix as output.
## First we fetch the cached result and if it's null then we compute the inverted matrix and store it for future use
## else we simply return the cached result.

cacheSolve <- function(x, ...) {
	im <- x$getSolution()
	if(!is.null(im)) {
		message("getting cached data")
		return(im)
	}
	data <- x$get()
	im <- solve(data, ...)
	x$setSolution(im)
	im
}
