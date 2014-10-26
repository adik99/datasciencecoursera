#Task to create functions to cache the inverse of Matrix.
#Date:- 27 October 2014
#Functions makeCacheMatrix and cacheSolve are so as to return inverse of matrix(m) where
#m<-makeCacheMatrix(x)  where x is a matrix(assumed to it can be  inverted to matrix m)
#cacheSolve(m) gives the inverted matrix by checking whether the inverted matrix is new or is already cached.




#Function makeCacheMatrix uses lexical scooping where the values of x and M are 
#assigned the values in parent frame so as to assign the values by check where the matrix is a new matrix 
#If it is a new matrix assign the Default NULL value.
#If it is cached then return the cached value.
#==================================== Function makeCacheMatrix =============================

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL # to set the value of m to NULL as default if cacheSolve has not been used
  set<-function(y){
    x<<-y #to set or assign default value other than the calling enviornment so as to identify if the value is cached
    m<<-NULL #to set or assign default value other than the calling enviornment so as to identify if the value is cached
  }
  get<-function() x
  setmatrix<-function(solve) m <<- solve # to return the inverse of the matrix
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

#==================================== Function cacheSolve =============================


#Function cacheSolve calculates the inverse of Matrix x by checking if it inverse is already calculated and cached.
#if the inverse is not calculated and the matrix is passed for the first time it will calulate inverse by 
#using get() function 
#

cacheSolve <- function(x, ...) {
  m<-x$getmatrix() #assign the inverted matrix to m
  if(!is.null(m)){ #check if the value of m is already cached.
    message("getting cached data")
    return(m)
  }
  matrix<-x$get() #if m is not calculated as inverse matrix and is NULL
  m<-solve(matrix, ...) #calculate the inverse of matrix and assign to m
  x$setmatrix(m) 
  m #return the inverted matrix
}

#============================== END of File =============================