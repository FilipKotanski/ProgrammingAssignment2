## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(mat = matrix()) {

  inverse_mat<-NULL 
  
  ##  defines a function to set the matrix 'mat' to a new matrix 'a' and resets the inverse matrix 'inverse_mat' to NULL
  
  set<-function(a) {
    mat<<-a
    inverse_mat<<-NULL
    
  }
  
  ##defines a function, which returns the matrix 'mat'
  
  get<-function() mat
  
  ##defines a function, which sets the matrix 'inverse_mat' to 'inverse'
  
  setinverse<-function(inverse)inverse_mat<<-inverse
  
  ##defines a function, which return the matrix 'inverse_mat'
  
  getinverse<-function() inverse_mat
  
  ##returns a list of functions/methods which an object initiated by makeCacheMatrix is going to have
  
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
  
  
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  ##getting the inverse of 'x'
  
  inverse_mat<-x$getinverse()
  
  ##checking if the inverse of 'x' had been initiated/calculated
  ##it is returned if exists
  
  if(!is.null(inverse_mat)){
    message('getting cached data')
    return(inverse_mat)
  }  
  
  ##otherwise original matrix  is extracted and passed to 'mat' variable
  
  mat<-x$get()
  
  ##checking if object 'mat' is a matrix
  
  if(!is.matrix(mat)){
    message('the given object is not a matrix')
    return(NULL)
  }
  
  ##checking if 'mat' is a square matrix
  
  if(nrow(mat)!=ncol(mat)){
    message('matrix is not square and cannot be inverted')
    return(NULL)   
  } 
  
  ##checking absolute value of the determinant of the matrix 'mat' 
  ##or more precisely the logarithm of the absolute value of the determinant
  ##to check if matrix 'mat' is invertible
  
  if(!is.finite(determinant(mat)$modulus)){
    message('determinant of matrix is 0 and matrix is not invertible')
    return(NULL)
  }  
  
  ##calculating the inverse of 'mat' and assigning it to variable 'inverse'
  
  inverse<-solve(mat,...)
  
  ##setting the value of inverse of object x/original matrix 'mat'
  
  x$setinverse(inverse)
  
  inverse
  
}
