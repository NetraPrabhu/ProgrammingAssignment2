## Put comments here that give an overall description of what your
## functions do


## Write a short comment describing this function
#Inputting a matrix that is invertible(can be checked by the condition mentioned above) and cacheing its inverse by using the following function makeCachematrix
makeCacheMatrix <- function(x = matrix()) {
        #Initially set to NULL
        m <- NULL
        #set function that sets the matrix
  set <- function(y){
    x <<- y
    m <<- NULL
  }
   # get function
   # gets the matrix
  get <- function() x
        #set the inverse of matrix by following function
  setinverse <- function(inverse) m <<- inverse
        # get the inverse
  getinverse <- function() m
        # Create a list of all the above functions namely set , get , setinverse , getinverse
  list(set = set , get = get , setinverse = setinverse , getinverse = getinverse )

}


## Write a short comment describing this function
#The inverse of the matrix is already been calculated by the above function now we will retrive the inverse from the cache
#cacheSolve is a function that will return inverse of matrix that has been inputted in makeCacheMatrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        #See if the inverse  is calculated or not
        m <-  x$getinverse ()
        # If it is calculated
  if(!is.null(m)) {
    message("getting cached data")
          # Return the computed inverse
    return (m)
  }
        #If it is not then..
        #Get the matrix
  data <- x$get()
        #Find inverse
  m <- solve(data , ...)
  x$setinverse(m)
        # Return the result
  m
}
