## These two functions enable efficient matrix inverse computation by 
## creating a cache-enabled matrix object and using cached data to avoid redundancy

## makeCacheMatrix creates a matrix object that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
	set <- function(y) {
	x <<- y
	inv <<- NULL
}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set = set, get = get, 
	setinverse = setinverse,
	getinverse = getinverse)
}


## cacheSolve computes a matrix's inverse, using cached data if possible

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
	if(!is.null(inv)) {
	message("getting cached data")
	return(inv)
}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinverse(inv)
	inv   
}
