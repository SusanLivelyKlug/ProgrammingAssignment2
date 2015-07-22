##
## slk 7/20/15
##
## makeCacheMatrix: creates a special "matrix" object that can cache
## it's inverse.
##
## cacheSolve: computes the inverse of the special cached "matrix"
## if the inverse has already been calculated and the matrix not
## changed, then cacheSolve retrieves the inverse from the cache

##
## makeCacheMatrix will take the matrix input x, and save a cache
## of the input.
## provides calls to
##        get : get the value of the matrix
##        set : set the value of the matrix
##        setinverse : saves an inverse of the matrix
##        getinverse : returns the inverse of the matrix
##
##
makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y) {
          x <<- y
          inv <<- NULL
     }
     get <- function() x
     setinverse <- function(inverse) inv <<- inverse
     getinverse <- function() inv
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


##
## cacheSolve : accepts a matrix to attempt an inversion
## if the matrix has already been inverted, then return the
## cached inverted matrix.
## otherwise invert the matrix and return the inverted matrix.
##
## assume the matrix is always invertable
cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     ## can be done with "solve(x)"
     inv <- x$getinverse()
     if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
     }
     data <- x$get()
     inv <- solve(data)
     x$setinverse(inv)
     inv
}

## ------------------  TEST -----------------
# to test in console did a test case ala- Sebastian Pajuelo Ayuso
# in the forum threads, thanks Sebastian!
#
# source("cacheMatrix.R")
# mat <-matrix(c(1,0,5,2,1,6,5,4,0), nrow=3, ncol=3)
# > mat
# [,1] [,2] [,3]
# [1,]    1    2    5
# [2,]    0    1    4
# [3,]    5    6    0
# > matrixx <- makeCacheMatrix(mat)
# > matrixx$get()
# [,1] [,2] [,3]
# [1,]    1    2    5
# [2,]    0    1    4
# [3,]    5    6    0
# > matrixx$getinverse()
# NULL
# > cacheSolve(matrixx)
# [,1]       [,2]       [,3]
# [1,]  2.6666667 -3.3333333 -0.3333333
# [2,] -2.2222222  2.7777778  0.4444444
# [3,]  0.5555556 -0.4444444 -0.1111111
#
#
