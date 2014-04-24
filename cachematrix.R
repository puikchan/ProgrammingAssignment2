## Function makeCacheMatrix
## It takes an argument x of type numeric matrix
## It returns a list of 4 functions
## 	 list (set = set, get - get, getInv = set Inv, getInv = getInv)
## Example usage:
## a <- makeCacheMatrix(matrix(1:4,2))
## a$get()
## a$getInv()
## a$set(matrix(5:8,2))
## a$get()

makeCacheMatrix <- function(x = matrix()) {
         m <- NULL
         set <- function(y) {
                 x <<- y
                 m <<- NULL
         }
         get <- function() x
         setInv <- function(solve) m <<- solve
         getInv <- function() m
         list(set = set, get = get,
              setInv = setInv,
              getInv = getInv)
}


## Function cacheSolve -- returns a matrix that is the inverse of argument x
## Example usage:
## a <- makeCacheMatrix(matrix(1:4,2))
## cacheSolve(a)
## cacheSolve(a)
## a$getInv()
## b = a$getInv()
## a$get() %*% b

cacheSolve <- function(x, ...) {
   m <- x$getInv()             # query the x Inverse Matrix's cache         
   if(!is.null(m)) {           # if there is a cache
     message("getting cached data") 
     return(m)                 # just return the cache, no computation needed
   }
   data <- x$get()             # if there's no cache
   m <- solve(data, ...)       # we actually compute them here
   x$setInv(m)                 # save the result back to x's cache
   m                           # return the result
}
