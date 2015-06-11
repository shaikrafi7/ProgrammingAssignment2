##makeCacheMatrix stores four functions to get & set the passed matrix, 
##and cache the calculated inverse matrix via setinverse and getinverse

makeCacheMatrix <- function(x = matrix()) {
			m <- NULL
            set <- function(y) {
                    x <<- y
                    m <<- NULL
            }
            get <- function() x
			setinverse <- function(matrix) m <<- matrix
			getinverse<- function() m
            list(set = set, get = get,
                 setinverse = setinverse,
                 getinverse = getinverse)
}


## cacheSolve checks if inverse of the passed matrix is cached via getinverse() and if cached returns the cached value. 
## Else, calculates the inverse through solve(), caches it through setinverse() and returns the freshly calculated inverse value.  

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
		
		#check if value is cached
		m <- x$getinverse()
		if(!is.null(m)) {
          message("getting cached data")
          return(m)
		}
    
    #get the original matrix from makeCacheMatrix's get function
		data <- x$get()
    #calculate inverse
    m <- solve(data, ...)
    #set calculated matrix into cache
    x$setinverse(m)
    #return freshly calculated inverse matrix
    m
}
