## Two functions are defined:
## 'makeCacheMatrix' returns a custom matrix that allows to cache the inverse, 
## such that the inverse needs to be computed only once.
## 'cacheSolve' returns the inverse of the custom matrix that is returned by
## makeCacheMatrix.
## makeCacheMatrix <- function(x = matrix())
## Return a custom matrix that allows to cache the inverse, such that the 
## inverse needs to be computed only once. 
## Input:
## - x: a square matrix.
## Output:
## - The output is a list of four functions:
##   - set(x = matrix()): The matrix can be (re)set.
##   - get(): The actual matrix is returned.
##   - setinverse(y = matrix()): The inverse of the actual matrix is set.
##   - getinverse(): The inverse of the actual matrix is returned if it is set.
##       otherwise NULL is returned.

makeCacheMatrix <- function(x = matrix())
{
   inv<-NULL
   set<-function(y)
   {
     x<<-y
     inv<<-NULL
   }
   get<-function() x
   setInv<-function(inverse) inv<<-inverse
   getInv<-function() inv
   list(set=set,get=get,setInv=setInv,getInv=getInv)
}


## cacheSolve <- function(x, ...)
## Return the inverse of the matrix returned by the makeCacheMatrix.
## If the inverse is already computed before, that inverse will be used. The
## 'solve' function is used to compute the inverse. Any extra arguments provided
## with the ... are also passed to the 'solve' function. 


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getInv()
      if(!is.null(inv))
      {
        message("getting cached data")
        return(inv)
      }
      d<-x$get()
      inv<-solve(d,...)
      x$setInv(inv)
      inv

}
