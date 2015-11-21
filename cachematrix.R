## makeCacheMatrix creates a list containing the results of functions to set the value of the matrix, 
## get the value of the matrix, set (calculate) the inverse of the matrix, and get (retrieve stored value) the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL  ## define m as an empty matrix (default)
        setmatrix <- function(y) {  ## set the value of the matrix
                x <<- y   ## <<- assigns a value to an object in an environment that is different from the current environment --> assigns y the value of x
                m <<- NULL  ## assigns m the value of NULL
        }
        getmatrix <- function() x  ## get the value of the matrix
        setinv <- function(solve) m <<- solve  ## calculate the inverse of the matrix
        getinv <- function() m  ## get (retrieve stored value) the inverse of the matrix
        list(setmatrix = setmatrix, getmatrix = getmatrix, setinv = setinv, getinv = getinv)  ## create a list to store all relevant values used by the function
}



## cacheSolve prints the inverse of the matrix created with the makeCacheMatrix function - it may retrieve value from the list if it exists, or calculate the inverse using solve()

cacheSolve <- function(x, ...) {
        ## return a matrix that is the inverse of 'x'

   m <- x$getinv()  ## gets m which may me assigned to the value of the inverse of the matrix from makeCacheMatrix
        if(!is.null(m)) {   ## checks if a value exists
                message("getting cached data")  ## print this message indicating that the value exists
                return(m)  ## and print existing matrix inverse (no calculation needed)
        }
        data <- x$getmatrix()  ## else, if the inverse does not exist in the cache, gets the input matrix
        m <- solve(data, ...) ## solves for the inverse
        x$setinv(m) ## set the value to the list (in the cache) so it is available in the future 
        m  ## and prints calculated matrix inverse

}
