## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    #set function to assign value to 'x'
    #and to initialize 'inv' to NULL 
    set <- function(y) {
        x <<- y # store matrix 'x'       
        inv <<- NULL #inv holds inverse of matrix 'x'
                     #intialize it to NULL
    }
    
    #get function to return the matrix
    get <- function() x
    
    #set inverse of matrix 'x' to 'inv'
    setinv <- function(inv_matrix) inv <<- inv_matrix
    
    #get the inverse of matrix 'x' ie return inv
    getinv <- function() inv
    
    #at the end of function, return 
    #list of functions associated 
    #with makeCacheMatrix
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    #get the inv of matrix
    inv <- x$getinv()
    
    # if the inv of matrix is not
    #NULL then return the cached
    # inverse ie inv
    if(!is.null(inv)) {
        #display a message
        message('Getting cached inverse')
        #return the inverse 'inv'
        return(inv)
    }
    #otherwise ie if inv of matrix is 
    #NULL  then first get the matrix
    my_matrix <- x$get()
    #compute the inverse of the matrix
    inv <- solve(my_matrix,...)
    #set the inv of the matrix using the
    #setinv function defined in makeCacheMatrix.
    #So next time when cacheSolve is called 
    #on this matrix, cached value will be returned
    x$setinv(inv)
    #return the inverse of the matrix 'inv'
    inv
}
