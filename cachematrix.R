# The two functions below are used to avoid computing a matrix inverse
# once it's already been computer.
# To run these two functions, first run makeCacheMatrix on a square
# invertible matrix and assign it to a variable. Then run cacheSolve
# on the variable.
# For example,
# b <- matrix(1:4,2,2)
# cacheSolve(b)
#
# makeCacheMatrix takes in a matrix and outputs a list containing four
# functions namely: set, get, setInv, and getInv.
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	# assigns NULL to a free variable m
	set<-function(y){
		x <<- y
		# Sets another matrix y equal to x. This changes the input.
		m <<- NULL
		# Assigns a value of NULL to a variable m from another environment
	}
	get <- function() x
	# Displays the matrix
	setInv <- function(solve) m <<- solve
	# Assigns computer inverse to the variable m from another environment
	getInv <- function() m
	# Displays the inverse of matrix x
	list(set=set, get=get,setInv=setInv,getInv=getInv)
	# Outputs the four functions
}
# cacheSolve checks to see if the matrix inverse has already been computed
# If it has, it retrives it from the cache and displays it. If it has not,
# it computes it and displays the result.
cacheSolve <- function(x=matrix(), ...) {
	m <- x$getInv()
	# Retrieves the inverse matrix if assigned
	# This would either yield the actuall inverse or NULL
	if(!is.null(m)){
		# Conditional statement. If there is an assigned value,
		# the inverse would not be computed and displays m.
		message("Getting cached inverse...")
		return(m)
	}
	matrix <- x$get()
	# Retrieves the original input and assigns it to a variable matrix
	m <- solve(matrix, ...)
	# Computes the matrix inverse.
	x$setInv(m)
	# Assigns the computer inverse to global variable m
	m
	# Displays the inverse
}