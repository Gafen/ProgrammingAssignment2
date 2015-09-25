## function that caches a matrix related values

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(y){
    x<<-y
    i<<-NULL
  }
  get<-function() x
  setinv<-function(inv) i<<-inv
  getinv<-function() i
  list(set=set,
       get=get, 
       setinv=setinv,
       getinv=getinv)
  
}


##  function that calculates and caches the inverse of a square matrix if it is not cached

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i<-x$getinv()
  if(!is.null(i)){
    message("getting cached inverse")
    return(i)
  }
  data <- x$get()
  i<-solve(data,...)
  x$setinv(i)
  i
}

