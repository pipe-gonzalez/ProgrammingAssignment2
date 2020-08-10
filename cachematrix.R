## In general terms, we have two functions here:
##the makeCacheMatrix, that creates an object that has a matrix and its inverse.
##The second one, the cacheSolve, what do is to calculate the inverse of a matrix
##only if that calculation was not already in memory. If it was already in memory,
##will show a message saying that got the result from cache.


## This first function, makeCacheMatrix define 4 funtions: 2 "gets" and 2 "sets"
## and it returns those functions within a list to the parent environment.
## 

makeCacheMatrix <- function(x = matrix()) {
            mi <- NULL
            set <- function(y) {
                  x <<- y
                  mi <<- NULL
            }
            get <- function() x
            setInv <- function(inv) mi <<- inv
            getInv <- function() mi
            list(set = set, get = get,
                 setInv = setInv,
                 getInv = getInv)
      
}


## This function, the cacheSolve use an imput argument of the type of
## makeCacheMatrix. Once have the argument it check if the "getInv" is NULL or
## not. If is not NULL, it means that it has already the calculations of the 
## inverse matrix and will give a message indicating that it is getting the
## results from cache and return the value. If is NULL then take the information
## of the original matrix (from x$get() ) and using the solve() function
## calculate the inverse matrix, send the value to the x$setInv and print the
## value. In that way, the next time that will go to calculate the inverse of
## that matrix will be already stored in cache and do not have to recalculate
## the results.

cacheSolve <- function(x, ...) {
      mi <- x$getInv()
      if(!is.null(mi)) {
            message("getting cached data")
            return(mi)
      }
      data <- x$get()
      mi <- solve(data, ...)
      x$setInv(mi)
      mi
}

