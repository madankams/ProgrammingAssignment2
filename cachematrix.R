## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly (there are 
## also alternatives to matrix inversion that we will not discuss here). 
## My assignment is to write a pair of functions that cache the inverse of a matrix


## Function to create CacheMatrix

makeCacheMatrix <- function(x = matrix()) {
  if (!exists("InverseMatrix")){
    InverseMatrix<<-NULL 
  }
  
  if (!exists("OrgMatrix")) {
    OrgMatrix <<- NULL
  }
  
  set <- function(y){
    OrgMatrix <<- y
    InverseMatrix <<- NULL
  }
  get <- function() OrgMatrix
  setInvers <- function (InvM) InverseMatrix <<- InvM
  getInvers <- function() InverseMatrix
  
  list(set=set, get=get, setInvers=setInvers, getInvers=getInvers)
}


## Function to pull from cache if available and if not calculate and place in cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  Tmp<-makeCacheMatrix()
  
  InverseMatrix <- Tmp$getInvers()
  if (!is.null(InverseMatrix)){
    OrgMatrix <- Tmp$get()
    Comp <- x[x == OrgMatrix]
    if (length(x)==length(Comp)){
      message("Getting cache data")
      return (InverseMatrix)
    }
    else {
      InverseMatrix<-solve(x)
      Tmp$set(x)
      Tmp$setInvers(InverseMatrix)
      InverseMatrix
    }
  }
  else {
    InverseMatrix<-solve(x)
    Tmp$set(x)
    Tmp$setInvers(InverseMatrix)
    InverseMatrix
  }
}
