## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(m = matrix()) {
	invertedMatrix <- NULL
	set <- function(y) {
		m <<- y
		invertedMatrix <<- NULL
	}
	get <- function() m
	storeSolution <- function(solution) invertedMatrix <<- solution
	invertMatrix <- function() invertedMatrix
	list(set = set, get = get,
			storeSolution = storeSolution,
			invertMatrix = invertMatrix)
}


## Write a short comment describing this function

cacheSolve <- function(m, ...) {
	invertedMatrix <- x$invertMatrix()
	if(!is.null(invertedMatrix)) {
		message("getting cached data")
		return(invertedMatrix)
	}
	data <- m$get()
	invertedMatrix <- solve(data, ...)
	m$storeSolution(invertedMatrix)
	invertedMatrix
}
