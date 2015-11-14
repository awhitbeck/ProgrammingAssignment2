######################################
######################################
# This file implements functions to facilitate cache of inverted matrices.
# There is one function, makeCacheMatrix, that will return a list of functions 
# for storing and getting the target matrix and the inverted target matrix.  
# There is one function, cacheSolve, which will use the standard R function, 
# solve, to invert the matrix if the inverted matrix is not already cached.
#
#Author: Andrew Whitbeck - November 14, 2015
#
######################################
######################################


#=====================================
# makeCacheMatrix - 
#-------------------------------------
# stores a matrix and implements four functions for facilitating caching of
# the inverted of the matrix.
# 
# Functions:
# - - - - - - - - - - - - - - - - - - 
# set(matrix) : stores matrix at m in functions environment 
# get() : returns m that is intended to be inverted
# getInverted(matrixInverted) : returns m^-1
# setInverted() : saves m^-1 as mInv in the functions environment
makeCacheMatrix <- function(x = matrix()) {
  
  mInv<-NULL 
  
  set <- function( yMatrix ){
    x <<- yMatrix
    mInv <<- NULL
  }
  get <- function() x
  setInverted <- function(matrixInverted) mInv <<- matrixInverted
  getInverted <- function() mInv
  
  list( set = set ,
        get = get , 
        getInverted = getInverted , 
        setInverted = setInverted )

}

#=====================================
# cacheSolve - 
#-------------------------------------
# takes a 'makeCachMatrix', inverts it if the cache is empty and caches the 
# result.  If the cache is not empty, it simply returns the cached mInv.
# 
cacheSolve <- function(x, ...) {

  if( is.null(matrixCache$getInverted()) ){
    mInv <- solve( matrixCache$get() , ... )
    matrixCache$setInverted( mInv )
  }
  
  matrixCache$getInverted()

}
