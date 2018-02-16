## The following functions will determine and store in cache the inverse matrix
## of a given invertible matrix


makeCacheMatrix <- function(x = matrix()) {
  ## This function  creates a list of 4 functions set, get, setinv, getinv

      inv <- NULL
      set <- function(y){   ## 1. to set the value of the matrix
        x <<- y
        inv <<- NULL
      }
      
      get <- function() x    ## 2. get the value of the matrix
      
      setinv <- function(solve) {   ## 3. set the value of the inverse matrix
         inv <<- solve
      }
      
      getinv <- function() inv   ## 4. get the value of the inverse matrix
      
      list(set=set, get=get, setinv=setinv, getinv=getinv)
      
}


## This function receives the list of functions created by MakeCacheMatrix and 
## return the inverse of the matrix given to that function

cacheSolve <- function(x, ...) {

  inv <- x$getinv()   ## Get the inverse of the matrix stored in 'x'
  
  if (!is.null(inv)){   ##If the inverse has already been calculated 
                        ##it just returns it from the cache
      message("getting cached data")
      return(inv)
  }
  
  data <- x$get() ## Get the value of the original matrix stored in 'x'
  
  inv <- solve(data,...)  ## Calculates the inverse of the matrix
  
  x$setinv (inv)  ## Set the value of the inverse matrix
  
  inv
}
