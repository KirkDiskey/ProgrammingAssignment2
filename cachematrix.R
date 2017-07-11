##I have functions to set the matrix and the inverse of the matrix and then I put these into a list

makeCacheMatrix <- function(x = matrix()) {
        inverse_matrix <- NULL
	#This sets the original matrix	
        set_matrix <- function(y) {
                x <<- y
                inverse_matrix <<- NULL
        }
        
        
        # This gets the original matrix
        get_matrix <- function() x
        
        # This sets the inverse of the matrix
        setinv <- function(inverse) inverse_matrix <<- inverse
        
        # This gets the inverse matrix
        getinv <- function() inverse_matrix
        
        # Place these into a list
        list(set_matrix = set_matrix, get_matrix = get_matrix,
             setinv = setinv,
             getinv = getinv)	
}

## cacheSolve function is used to derive the inverse and store the results
##When I use cacheSolve again on the same matrix, a message appears saying that the cached matrix is being used.

cacheSolve <- function(x, ...) {
        # This returns the inverse matrix of x and determines if it has been used yet
        inverse_matrix <- x$getinv()
        
        # If it has already been used, then it will use it and provide a message stating that it is reusing the matrix
        if(!is.null(inverse_matrix)) {
                # Simply return the computed inverse		
                message("Ensuring cached matrix is being used on the subsequent runs")
                return(inverse_matrix)
        }
        
        # If it hasn't already been used, then it will be computed and used
        data <- x$get_matrix()  ########may need to change this back
        
        inverse_matrix <- solve(data, ...)
        
        x$setinv(inverse_matrix)
        
        inverse_matrix    


# To test:
# x <- matrix(1:4, nrow=2, ncol=2)
# a <- makeCacheMatrix(x)
# b <- cacheSolve(a)
# b should return:
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#
# b2 <- cacheSolve(a)
# This should display a "Ensuring cached matrix is being used on the subsequent runs" message
# b2 should return
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5

}