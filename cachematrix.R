
## The aim is to get the inverse of a matrix, following the example of the mean I got this.


makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y){
		x <<- y
		m <<- NULL
	}
	get <- function()x
	setinv <- function(solve) m <<- solve
	getinv <- function()m
	list(set=set, get=get, 
		setinv = setinv,
		getinv = getinv) 
}

#x<- rbind(c(1,-1/4),c(-1/4,1))
#solve(x)

#makeCacheMatrix(x)$setinv(solve(x))
#makeCacheMatrix(x)$getinv()

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
	m<- x$getinv()
	if(!is.null(m)){
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data,...)
	x$setinv(m)
	m ## Return a matrix that is the inverse of 'x'
}

cacheSolve(makeCacheMatrix(x))


