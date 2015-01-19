## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix function is modelled on the makeVector function in the assignment
## It creates a special "matrix" object that can cache its inverse (the solve function)

makeCacheMatrix <- function(x = matrix()) {

m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatric. If the inverse
## has already been calculated and is in the cache, it retrieves and displays the cache. Otherwise it is calculated.
## If the matrix changes, the inverse is obviously calculated (and not in the cache)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
         m<-x$getmatrix()
  if(!is.null(m)){
    message ("Loading previously cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}
