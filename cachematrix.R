## There are two functions in this file "makeCacheMatrix" and
## "cacheSolve"


makeCacheMatrix <- function(x = matrix()) {
## "makeCacheMatrix" takes an invertible matrix "x" as argument;
## then, it produces an object of the type "makeCacheMatrix",
## which is a list with four functions: "set_matrix"
## "get_matrix", "set_inv", and "get_inv".
## For example, if a random object "A" is a matrix, 
## then "m.object<-makeCacheMatrix(A)" produces an object type
## "makeCacheMatrix" with name "m.object"    
        
        # "x_inv" is the inverted matrix of "x"
        # Initially, it is created as a NULL object
        x_inv <- NULL
        
        ## "set_matrix" assigns the initial value to "x" and "x_inv"
        ## "x" and "x_inv" values are assigned to parent environment
        set_matrix <- function(y) {
                x <<- y
                x_inv <<- NULL
        }
        
        ## "get_matrix" obtains a matrix from the value of "x"
        get_matrix <- function()
                x
        
        ## "set_inv" assigns the value of the inverted matrix "x_inv"
        ## "x_inv" gets value from "inv_matrix" argument, and assigns
        ## its value to the parent environment
        set_inv <- function(inv_matrix)
                x_inv <<- inv_matrix
        
        ## "get_inv" obtains the inverted matrix from the value of "x_inv"
        get_inv <- function()
                x_inv
        
        ## Creates a list of four objects and assigns a name to each one
        list(
                set_matrix = set_matrix,
                get_matrix = get_matrix,
                set_inv = set_inv,
                get_inv = get_inv
        )
}



cacheSolve <- function(x, ...) {
## The function "cacheSolve" can calculate, or retrieve from the cache,
## the inverse of a matrix "x" of the type type "makeCacheMatrix"
## For example, if "m.object" is an object produced using the function
## "makeCacheMatrix", then cacheSolve(m.object) would first evaluate if
## there is already an inverted matrix calculated in the cache, otherwise
## it will calculate the inverse and store if in the cache
        
        ## First "x_inv" is read from function "get_inv()" in "x" 
        x_inv <- x$get_inv()
        
        ## the function checks if "x_inv" is not NULL
        ## and if it already has a value, it returns the matrix "x_inv"
        ## A message showing "getting cached data" indicates that
        ## the value shown is not a new calculation, but a retrieval
        ## from the cache
        if (!is.null(x_inv)) {
                message("Getting cached data")
                return(x_inv)
        }
        
        ## If "x_inv" was NULL, then matrix data is read
        ## from the input "x" using function "get_matrix()",
        ## then, it assigns the matrix to object "data",
        ## calculates the inverse of the matrix with "solve()",
        ## and assigns the solution to the input "x"
        ## using the function "set_inv()"
        ## Finally, it returns the result
        ## A message showing "Calculating data..." indicates
        ## that a new calculation is being performed
        data <- x$get_matrix()
        x_inv <- solve(data, ...)
        x$set_inv(x_inv)
        message("Calculating data...")
        return(x_inv)
}
