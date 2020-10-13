makeCacheMatrix<-function(x=matrix()){
      inv<- NULL
      Set <-function(y){
        X <<-y
        inv <<- NULL
      }
      get<- function() {x}
      SetInverse <- function(inverse){inv <<-inverse}
      getInverse <- function(){inv}
      list(Set = Set, get = get, SetInverse = SetInverse, getInverse = getInverse)
}

cachesolve <- function( x,...){
     inv <- x$getInverse()
     if(!is.null(inv)){
           message("getting cached data")
           return(inv)
     }
     mat <- x$get()
     inv <- solve(mat,...)
     x$SetInverse(inv)
     inv
}