## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
     #Initialize the matrix that holds the inverse of x within the function\\
     #environment
     inv <- NULL
     
     #This function initializes 'x'(matrix) and 'inv'(inverse matrix) on the\\
     #parent directory (where the function is called form) 
     set <- function(y) {
         x <<- y
         inv <<- NULL
     }
 
     #This function retrives the value of 'x'(matrix) within the function\\
     #environment
     get <- function() {x}
     
     #This function sets the value of 'inv' in the parent directory\\
     #to the inverse matrix
     setinv <- function(inverse) {inv <<- inverse}
     
     #This function retrieves the value of 'inv'(inverse) within the function\\
     #environment
     getinv <- function() {inv}
     
     list(set = set, 
          get = get,
          setinv = setinv,
          getinv = getinv) 
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
     #Gets the value of inv forn the input object x
     inv <- x$getinv()
     
     #Checks if the 'x' object has been updated from the last time the \\
     #CacheSolve function ran. Everytime the 'x'is redefined it sets the \\
     #'inv' to NULL. If it is not NULL then it just caches the last value\\
     #of inv and exits the function
     if(!is.null(inv)) {
         message("getting cached data")
         return(inv)
     }
     #If the 'x' object has been updated from the last time the \\
     #CacheSolve function ran, then calculate the inverse matrix
     data <- x$get()            #retrieve the matrix
     inv <- solve(data, ...)    #find the inverse
     x$setinv(inv)              #set or 'store' the inverse matrix on object 'x'
     inv                        #Return the inverse matrix
}
