## Pair of functions that cache the inverse of a matrix.
## Checks the cache to see if the inverse has already been computed for the matrix 
## in question. If already computed, it returns the cached value saving some 
## computation time. If not already computed, computes and returns the answer using solve().


## This function creates a special "matrix" from the matrix x that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        ## Sets cached inverse of the matrix to NULL
        inv_matrix <- NULL
        ## Sets value of the special "matrix" for matrix x
        ## Substitutes new value of x with y from the input environment
        ## inv_matrix <<- NULL restores the value of inv_matrix when not required
        set_matrix <- function(y) {
                x <<- y
                inv_matrix <<- NULL
        }
        ## Gets matrix x from makeCacheMatrix function to calculate the inverse
        get_matrix <- function() x
        ## Sets the value of the inverse
        set_inv <- function(solve) inv_matrix <<- solve
        ## Gets the value of the inverse
        get_inv <- function() inv_matrix
        ## Creates a function list which makes up the special "matrix"
        list(set_matrix = set_matrix, get_matrix = get_matrix, set_inv = set_inv, get_inv = get_inv)
}


## Checks the inverse of the special "matrix" created by the makeCacheMatrix function
## against its cache. If it exists in the cache, cached inverse returned. If it does not
## exist, inverse calculated and returned.

cacheSolve <- function(x, ...) {
        ## Checks if inverse exists with get_inv function
        ## If not NULL print message and return cached inverse (inv_matrix)
        inv_matrix <- x$get_inv()
        if(!is.null(inv_matrix)) {
                message("Getting cached data")
                return(inv_matrix)
        }
        ## Else, gets the data from get_matrix into matrix_data
        matrix_data <- x$get_matrix()
        ## New inverse of matrix (inv_matrix) calculated and set
        inv_matrix <- solve(matrix_data, ...)
        ## set_inv stores inv_matrix in the object generated with makeCacheMatrix
        x$set_inv(inv_matrix)
        ## Newly calculated inverse returned
        inv_matrix
}
