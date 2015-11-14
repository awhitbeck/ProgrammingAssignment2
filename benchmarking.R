#########################
# bench marking for programming assignment 2 -
# this script will load the functions that should 
# have been implemented for programming assignment
# 2 and will test several matrices to ensure that 
# the code is working properly
##########################

# generate 10 random matrices of size 2x2 
# check if the matrix is singular (det(matrix)==0)
# invert with test functions
# check that the inverted matrix time the uninverted matrix multiplies out to [I]

source("/Users/awhitbe1/Coursera/JHUdataScienceSpecialization/courses/02_RProgramming/progAssignment2/ProgrammingAssignment2/cachematrix.R")

indentityCheck <- function( m ){
  
  if( nrow( m ) != ncol( m ) ){
    print("indentityCheck: matrix not square")
    return(FALSE)
  }
  
  #loop over each element of the matrix and see if its either 1 or zero within tolerance
  for( i in 1:nrow(m) ){
    for( j in 1:nrow(m) ){
      if( i == j & ( m[i,j] > 1.0000001 | m[i,j] < .9999999   ) ) return(FALSE)
      if( i != j & ( m[i,j] > 0.0000001 | m[i,j] < -0.0000001 ) ) return(FALSE)
    }# end loop over columns
  }# end loop over rows
  
  return(TRUE)
  
}

for( j in 1:100 ){
  
  i<-0

  while( i <= 1 ){
    
    # generate random matrix
    m <- matrix( sample( 1:1000000 , j*j ) , j , j )
    # check if random matrix is non-singular
    if( det( m ) != 0 ){
      # matrix is non-singular, so invert!
      mCache <- makeCacheMatrix( m )
      mInv <- cacheSolve( mCache )
      if( !indentityCheck( mInv %*% m ) ){
        print("matrix inversion failed!!")
        print( mInv %*% m )
      }

    }else{
      next()
    }
    
    i <- i+1
    
  }#end loop over ith random matrix
}#end loop over matrices of size j