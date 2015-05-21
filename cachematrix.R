makeCacheMatrix <- function(x = matrix()) { 

	# holds the cached value or NULL if nothing is cached 
      # initially nothing is cached so set it to NULL

     i <- NULL 
	#store a matrix
     set <- function(y) { 
         x <<- y 
         i <<- NULL 
     } 
	# returns the stored matrix
     get <- function()x 

	# cache the given argument
     setinverse <- function(solve)
	 i <<- solve 
	
	# get the cached value
     getinverse <- function()i 

     list( 
         set = set, 
         get = get, 
         setinverse = setinverse, 
         getinverse = getinverse 
     ) 
 } 


The following function calculates the inverse of a "special" matrix created with  
 # makeCacheMatrix 

cacheSolve <- function(x, ...) { 
	# get the cached value 
     i <- x$getinverse()
	# if a cached value exists return it  
     if(!is.null(i)) { 
         message("getting cached data") 
         return(i) 
     } 
	# otherwise get the matrix, caclulate the inverse and store it in the cache
     m <- x$get() 
     i <- solve(m, ...) 
     x$setinverse(i) 
 	# return the inverse
     i 
 } 

#executes the function
s <- makeCacheMatrix(matrix(c(2, 12, 1, 2), c(2, 2))) 
cacheSolve(s)

