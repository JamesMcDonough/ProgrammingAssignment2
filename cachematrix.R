## cachematrix.R
## ProgrammingAssignment2

# ==========================================================================
# Write the following functions:
  
# 1.  `makeCacheMatrix`: This function creates a special "matrix" object
# that can cache its inverse.

# 2.  `cacheSolve`: This function computes the inverse of the special
# "matrix" returned by `makeCacheMatrix` above. If the inverse has
# already been calculated (and the matrix has not changed), then
# `cacheSolve` should retrieve the inverse from the cache.
# ==========================================================================

# This is a function that takes a matrix x and returns 
# a list with 4 functions in it: set, get, setinv, getinv
makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {
    # defines a function to set the matrix, x, to a new matrix, y, and 
    # resets the inverse, inv, to NULL 
    x <<- y 
    inv <<- NULL 
  }
  # note same code as in the ReadMe example
  # returns the matrix, x
  get <- function() x
  # sets the inverse, inv, to inverse
  setinv <- function(inverse) inv <<- inverse 
  # returns the inverse, inv
  getinv <- function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

# This function returns the inverse of a matrix. If 
# the input matrix has been passed through makeCacheMatrix 
# it will return the cached value if available

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  # the inverse of a matrix is returned by solve(x)
  
  # calls the getinv function from above. Visibility??
  # dollar sign accesses that column i.e. from the 4 functions in the list
  inv = x$getinv()
  
  # if inv is not null then the calculated value has already been stored in cache 
  if (!is.null(inv)){
    # no need to calculate it again
    message("retrieving cached value")
    return(inv)
  }
  
  #solve(x)
  
  # if not, call the function to do the calculation 
  matrix = x$get()
  inv = solve(matrix, ...)
  
  # if you have calculated then set the value in cache
  x$setinv(inv)
  
  # either way, return the value of inv
  return(inv)
}

# =======================================================

# # Test code
# 
# M <- matrix(c(5,0,0,5), nrow=2, ncol=2)
# print(M)
# 
# MC <- makeCacheMatrix(M)
# Minv <- cacheSolve(MC)
# print(Minv)
# 
# N <- matrix(c(3,2,4,5,6,4,5,-3, 0), nrow=3, ncol=3)
# print(N)
# NC <- makeCacheMatrix(N)
# Ninv <- cacheSolve(NC)
# print(Ninv)
# 
# # should be cached: should print 'retrieving cached value'  
# Minv <- cacheSolve(MC)
# print(Minv)

