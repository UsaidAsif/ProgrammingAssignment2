# There are a total of two functions which make use of R's lexical scooping rules and the fact that lists'
# elements can be set to functions (since everything in R is an object, and functions too are objects). According
# to https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprogAssignment2Prototype.md, 
# the functions in the list are the equivalent of methods in a Java class (and thinking it this way have helped my
# understanding of R's lexical scooping rules.) and hence the list behaves as a Java 'static' class, with calling 
# the functions using a $ sign instead of a dot as is done in the Java programming language.

# makeCacheMatrix initializes a default matrix (if not given as an argument to the function when called) and
# then sets the inverse to NULL. It has functions to set the matrix, get the matrix, set the inverse and get
# the inverse defined in its own environment. These functions indeed take valid arguments to set/get their
# values. The last line returns a list whose elements are set to function calls so that we may use the $
# operator to use the getter and setter functions. The setmatrix changes the current matrix used by the 
# makeCacheMatrix() and thus uses the <<- operator to define its value since it needs to modify the
# value of our_matrix variable in the parent environment (which means inside the makeCacheMatrix). 

makeCacheMatrix <- function(our_matrix = matrix()) {
  
  inv <- NULL
  setmatrix <- function(newmatrix) {
    our_matrix <<- newmatrix
    inv <<- NULL
  }
  getmatrix <- function() our_matrix
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinv = setinv,
       getinv = getinv)
  
}


# cacheSolve takes an object of type makeCacheMatrix (or more appropriately/exactly a list returned by a 
# previous call to makeCacheMatrix). If some other argument is passed to this function such as any atomic
# vector so it returns an error since that atomic vector does not have the getter and setter functions
# which are used inside the method. First the inverse is checked that whether any previous value is found
# or not, if found so it returns that value and does not uses the solve function to find the inverse which
# is a costly operation for larger matrices. x$getmatrix() returns the original matrix either passed previously
# to makeCacheMatrix or set via the $setmatrix function invoked on the list returned by the makeCacheMatrix.
# After using solve to find the inverse, the setinv() function is called to set the inverse so that later on
# the value is cached and not recalculated, and then finally also the inverse itself is returned by the function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$getmatrix()
  inv <- solve(mat)
  x$setinv(inv)
  inv
}

