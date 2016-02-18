# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly (there
# are also alternatives to matrix inversion that we will not discuss here). Your
# assignment is to write a pair of functions that cache the inverse of a matrix.
# 
# Write the following functions:
# 
# 1. makeCacheMatrix: This function creates a special "matrix" object that can
# cache its inverse. 
#
# 2. cacheSolve: This function computes the inverse of the
# special "matrix" returned by makeCacheMatrix above. If the inverse has already
# been calculated (and the matrix has not changed), then the cachesolve should
# retrieve the inverse from the cache. Computing the inverse of a square matrix
# can be done with the solve function in R. For example, if X is a square
# invertible matrix, then solve(X) returns its inverse.
# 
# For this assignment, assume that the matrix supplied is always invertible.


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inverse) inv <<- inverse
  
  getInverse <- function() inv
  
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  
  inv <- x$getInverse()
  
  # if Inverse Matrix is cound in the cache
  if (!is.null(inv)) {
    message("get the cached Data...")
    ret <- inv
    
  } else{
    
    ## get the basic matrix
    matr <- x$get()
    
    ## calculate the invers matrix with solve()
    ret <- solve(matr, ...)
    
    ## set die Invers Matrix
    x$setInverse(ret)
    
  }
  
  return(ret)
}


## now some testing
## -----------------------------

# Basic Test Matrix
basic_matrix <- matrix(rnorm(16),4,4)
basic_matrix 
#          [,1]       [,2]        [,3]       [,4]
# [1,] -1.1857570  0.6509791  1.53486312 -0.4394518
# [2,]  0.2559072 -1.8261497 -1.12531955 -0.2518908
# [3,]  0.1174074  0.9693064  0.04249344 -1.0454860
# [4,]  0.8891443 -0.3740881  2.68017314  0.1716959

# Create our <<special>> Matrix
cacheMatrix <- makeCacheMatrix(basic_matrix)

cacheMatrix$get() 
#          [,1]       [,2]        [,3]       [,4]
# [1,] -1.1857570  0.6509791  1.53486312 -0.4394518
# [2,]  0.2559072 -1.8261497 -1.12531955 -0.2518908
# [3,]  0.1174074  0.9693064  0.04249344 -1.0454860
# [4,]  0.8891443 -0.3740881  2.68017314  0.1716959

cacheMatrix$getInverse()
# NULL ## Thats right! 

cacheSolve(cacheMatrix)
#          [,1]         [,2]        [,3]        [,4]
# [1,] -0.6082001 -0.103191447  0.32973434  0.29974517
# [2,] -0.1729458 -0.494350948  0.17352582 -0.11127207
# [3,]  0.1917785 -0.004650155 -0.03647802  0.26190972
# [4,] -0.2208497 -0.470107287 -0.76006485 -0.05885777

cacheMatrix$getInverse()
#          [,1]         [,2]        [,3]        [,4]
# [1,] -0.6082001 -0.103191447  0.32973434  0.29974517
# [2,] -0.1729458 -0.494350948  0.17352582 -0.11127207
# [3,]  0.1917785 -0.004650155 -0.03647802  0.26190972
# [4,] -0.2208497 -0.470107287 -0.76006485 -0.05885777
