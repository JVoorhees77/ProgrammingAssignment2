# makeCasheMatrix creates a matrix, stores it, and permits 
# the creation and storage of the inverse matrix. Storing 
# the matrix and inverse matrix circumvents the creation
# of new matrices by drawing upon the stored matrix 
# objects as needed.



makeCacheMatrix <- function(x = matrix()) {
	inv_matrix <- NULL		# creates inverse matrix
	set <- function(y) {	# stores matrix
		x <<- y
		inv_matrix <<- NULL
	}
	get <- function() x									# gets matrix
	setinv <- function(inverse) inv_matrix <<- inverse	# stores inverse matrix
	getinv <- function() inv_matrix						# gets inverse matrix
	list(set = set, get = get, setinv = setinv, getinv = getinv)	# returns matrix as defined
}


# cacheSolve creates the inverse of the matrix formed by 
# makeCachematrix (above) when the inverse matrix has not 
# already been created and stored. When the inverse matrix 
# has already been formed by makeCachematrix, the inverse 
# matrix is retrieved.


cacheSolve <- function(x, ...) {
	inv_matrix <- x$getinv()
	if (!is.null(inv_matrix)) {				# retrieves inverse matrix if previously created
		message("getting cached data")
		return(inv_matrix)
	}
	data <- x$get()		# creates inverse matrix if not previously created 
	inv_matrix <- solve(data, ...)
	x$setinv(inv_matrix)	# stores inverse matrix
	inv_matrix		# gets inverse matrix
}
