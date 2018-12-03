## The purpose of the proram is to return the inverse of a matrix. If the 
## the same matrix inverse is requested again the program will return //
## the cached inverse instead of calculating the inverse matrixagain. If 
## the matrix is redefined, then ignores the cached value and  calculates the
## inverse of the newly defined matrix.

# makeCacheMatrix: Creates an object consisting of: 
#   'x'= an input matrix
#   'inv'= the input matrix inverse
#   'set()'= Function that sets x and inv=NULL in the parent environment.
#   'get()'= Retrieves the input matrix
#   'getinv()'= retrieves inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    
     inv <- NULL           #Initialize 'inv' in local environment
     
     #Sets 'x' and 'inv' in parent environment
     set <- function(y) {  
         x <<- y
         inv <<- NULL
     }
     
     #Retrieve matrix
     get <- function() {x}  
  
     #Sets inverse in parent directory\\
     setinv <- function(inverse) {inv <<- inverse} 

     #Retrieves the matrix inverse
     getinv <- function() {inv}
   
     #Object list 
     list(set = set, 
          get = get,
          setinv = setinv,
          getinv = getinv) 
}


## cacheSolve: Checks if the 'x' object has been updated from the last time \\
#the cacheSolve function ran. If it has not, then it retrieves the cached inverse\\
#matrix from 'x'. If 'x' has been updated, then in calculates the inverse matrix.

cacheSolve <- function(x, ...) {
     #Gets the value of inv from the input object 'x'
     inv <- x$getinv()
    
     #If 'x' has not been updated from the last time cacheSolve ran, then use\\
     #the cached inverse matrix 
     if(!is.null(inv)) {  
         message("getting cached data")
         return(inv)
     }
     #If 'x' has been updated from the last time cacheSolve function ran, then\\
     #calculate the inverse matrix
   
     data <- x$get()            #retrieve the matrix
     inv <- solve(data, ...)    #find the inverse
     x$setinv(inv)              #set or 'store' the inverse matrix in object 'x'
     inv                        #Return the inverse matrix
}
