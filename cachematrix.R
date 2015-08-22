## Put comments here that give an overall description of what your
## functions do
## The following two functions create cacheable inverse of a matrix. 
## Note/Assumption:- Input to makeCacheMatrix() function is always an
##    invertible square matrix.
## makeCacheMatrix() function creates cache entries for the matrix and its
##    inverse. It also caches matrix input and initializes inverse of this
##    matrix as NA. Note that this function does not compute inverse.
##
## cacheSolve() function returns either cached inverse of the object created
##    using makeCacheMatrix() if not NA. If inverse is not existing, it
##    computes inverse using R's solve() function and then saves using one
##    of the functions set_invm() of the object.

makeCacheMatrix <- function(x = matrix()) {
    ## x is a matrix input. If not provided an empty matrix is created.
    ## this function returns the object for the matrix and its inverse 
    ## cached values. This object provides functions that can operate on 
    ## this object are:
    ## get() - Gets cached matrix
    ## set() - Caches the matrix provided - Note that det() of matrix shall 
    ##    not be zero and rows and col must be equal are done and
    ##    it must be invertible square matrix
    ## get_invm() - Gets cached inverse matrix of x
    ## set_invm() - Caches inverse matrix of x
    ## returns list of these functions
  
    inv_matrix <- matrix()
    
    # operation functions
    set <- function(y) {
        # check input
        if (det(y) == 0 || y.nrow != y.ncol) {
            print("Error in input of makeCacheMatrix.get()!")
            return()
        }
        x <<- y  # caches input matrix
        inv_matrix <<- matrix()  # initializes empty matrix
    }
    get <- function() x
    set_inv_matrix <- function(invmatrix) inv_matrix <<- invmatrix
    get_inv_matrix <- function() inv_matrix
    list(set = set, get = get, set_invm = set_inv_matrix, 
         get_invm = get_inv_matrix)
}


## cacheSolve() function takes an object created using makeCaheMatrix().
## This function returns if cached inverse exists, if not, it will compute
## inverse on the matrix and caches inverse and then provides invese of 
## the matrix.

cacheSolve <- function(x, ...) {
    ## x is an object that has been created using makeCacheMatrix().
  
    # gets cached inverse in this object x
    invm <- x$get_invm()
    
    # Check to see if there exists valid cached inverse in this object
    if (!anyNA(invm)) {
        message("Cached inverse matrix")
        return(invm)
    }
    
    # Get matrix and calculate inverse
    newm <- x$get()
    
    # assuming ivertible square matrix and it returns inverse of newm
    newinvm <- solve(newm)
    # caches inverse of the matrix
    x$set_invm(newinvm)
    ## Return a matrix that is the inverse of 'x'
    newinvm
}
