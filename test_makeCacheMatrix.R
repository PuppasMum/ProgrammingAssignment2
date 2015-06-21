makeCacheMatrix <- function(x = matrix()) {
        inv_matrix <- NULL
        set_matrix <- function(y) {
                x <<- y
                inv_matrix <<- NULL
        }
        get_matrix <- function() x
        set_inv <- function(solve) inv_matrix <<- solve
        get_inv <- function() inv_matrix
        list(set_matrix = set_matrix, get_matrix = get_matrix, set_inv = set_inv, get_inv = get_inv)
}

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_matrix <- x$get_inv()
        if(!is.null(inv_matrix)) {
                message("Getting cached data")
                return(inv_matrix)
        }
        matrix_data <- x$get_matrix()
        inv_matrix <- solve(matrix_data, ...)
        x$set_inv(inv_matrix)
        inv_matrix
}