## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#Inputting a matrix that is invertible and cacheing its inverse by using the following function makeCachematrix
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        #set the value of the matrix
  set <- function(y){
    x <<- y
    m <<- NULL
  }
        
  get <- function() x
        #set and get the inverse of matrix
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set , get = get , setinverse = setinverse , getinverse = getinverse )

}


## Write a short comment describing this function
#The inverse of the matrix is already been calculated by the above function now we will retrive the inverse from the cache
#cacheSolve is a function that will return inverse of matrix that has been inputted in makeCacheMatrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <-  x$getinverse ()
  if(!is.null(m)) {
    message("getting cached data")
    return (m)
  }
  data <- x$get()
  m <- solve(data , ...)
  x$setinverse(m)
  m
}
