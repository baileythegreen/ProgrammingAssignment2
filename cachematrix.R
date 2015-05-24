## Put comments here that give an overall description of what your
## functions do

## Creates a matrix and functions to set and get the matrix. Caches the matrix so that it can be accessed more quickly later on.



makeCacheMatrix <- function(x = matrix()) 

	
      {     m <- NULL
            set <- function(y) 
               	   {x <<- y				
                    m <<- NULL}
            
            get <- function() 
				{x}
            setmatrix <- function(matrix) 
				{m <<- matrix}
            getmatrix <- function() 
				{m}
            list(set = set, get = get,
                 setmatrix = setmatrix,
                 getmatrix = getmatrix)			}		
			 												

# test matrix	
data1 <- matrix(c(2,1,0,0,3,0,2,2,1,3,-3,3,5,1,2,1), nrow = 4, ncol = 4)
data1
output1 = makeCacheMatrix(data1)

## Generates the inverse of the matrix from the first function, but first checks to see if this has already been done. If so, it pulls the previously calculated value.

cacheSolve <- function(x, ...) 
        ## Return a matrix that is the inverse of 'x'
		
	      {    m <- x$getmatrix()
	            if(!is.null(m)) 
	                	{message("retreiving inverse")
	                     return(m)}
            
	            i <- x$get()
	            m <- solve(i)
	            x$setmatrix(m)
	            m											}	
																
																	
cacheSolve(output1)