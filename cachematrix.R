makemat <- function(x = matrix()) {
     ## defines argument "x" as matrix   
	s <- NULL
			## define s and initialize as NULL
	set <- function(y) {
            ## define function "set"    
		x <<- y
                 ## y is assigned to x in x's parent environment
		s <<- NULL
              ## NULL is assigned to s in its parent environment
	}
        

	get <- function() x
        			## define function "get", returns "x"
	setsolve <- function(solve) s <<- solve
         ## define function "setsolve", assigns inverse of matrix to "s"
	getsolve <- function() s
        
	list(set = set, get = get,
             
	     setsolve = setsolve,
             
	     getsolve = getsolve)

}


cachesolve <- function(x = matrix(), ...) {

	s <- x$getsolve()
        			## if s is not null anywhere in the global environment
   							## then it returns the value of s
	if(!is.null(s)) {

        	message("getting cached data")

                return(s)
        
	}
        

	data <- x$get()
					## if s is not defined anywhere in the global environment
        s <- solve(data, ...)
				## then it assigns x to data, and then inverse of data to
        x$setsolve(s)
        				## s and returns that value.
	s

}

