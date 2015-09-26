######################################################################################
## We will follow very closely to what is being explained in the instructions for (1) makevector
## and (2) cachemean. Though, instead of using the mean() function, we will use the solve() function.
## The solve() function returns the inverse of a matrix.
## Essentially we will replace all refernces to mean() function with references to solve() function.
## For example, the "setmean <- function(mean) m <<- mean" will be replaced by
## setinverse <- function(solve) m <<- solve".
## Also note: cacheSolve() requires the matrix returned by makeCacheMatrix(). 
######################################################################################
## The makeCacheMatrix function is a function that stores/contains 4 other functions as well as a matrix = m.
## set() changes the matrix stored in the main function.
## get() returns the matrx in the main function
## setinverse() stores an input value in m
## getinverse() returns the value in m.
## Essentially, the are 4 functions in the makeCacheMatrix function.
## Nice explanation is here:https://github.com/DanieleP/PA2-clarifying_instructions.
## To use the functions stored in the main function one needs to subset the main function.
## We will do this later with b$get() and b$getinverse. Later, when we test functions,
## we will create b with b<- makeCacheMatrix(a). a will be the input matrix.
#########################################################################
## Write a short comment describing this function. Also see explanation above.

makeCacheMatrix <- function(x = matrix()) {

###initialize m to NULL in local enviornment  
  m <- NULL 
  
### sets the stored matrix in global enviornment 
  set <- function(y) { 
  x <<- y 
  m <<- NULL   ###sets m to NULL in global enviornment
  } 
  
### gets the stored matrix x in local enviornment
### it returns the matrix x stored in the main function. Doesn't require any input.
  
  get <- function() x
  
### sets m in local enviornment. Function solve() creates inverse
### According to explanation in https://github.com/DanieleP/PA2-clarifying_instructions
### setinverse ad getinverse are just used to set setinverse to m and m can be retrieved with
### function getinverse.  
  setinverse <- function(solve) m <<- solve
  
### gets m in local enviornment. 
  getinverse <- function() m
  
### lists the 4 functions so that tey are available to be accessed.
### To store the 4 functions in the function makeCacheMatrix we need the function list(), 
### so that when we assign makeCacheMatrix to an object, the object has all the 4 functions  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) 
  }


## Write a short comment describing this function
## This function returns the inverse of a matrix create in the makeCachMatrix() function.
## First, it will retrieve m if it has a stored value.


cacheSolve <- function(x, ...) {
     
## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    
## If m is not empty, return m, whatever it is set to.
    if(!is.null(m)) { 
    message("getting cached data.") 
    return(m) 
  } 
  
## If the matrix inverse has not been calculated, get the matrix stored in makeCacheMatrix().
## Calculate the inverse, set the inverse and return the inverse.
## data gets the vector stored with makeCacheMatrix, m calculates the inverse of the vector and 
## x$setinverse(m) stores it in the object generated assigned with makeCacheMatrix.
    
  data <- x$get() 
  m <- solve(data) 
  x$setinverse(m) 
  m
}

### Example 1:

a<- matrix(c(2,1,4,5,7,3,8,2,6), nrow=3, ncol=3)
a
b<- makeCacheMatrix(a)
cacheSolve(b)  ## returns inverse of a
b$get()   ## returns the original matrix a
b$getinverse() ## returns inverse of a

### Example 2: Other Example to Test.

d <- matrix(c(1,2,3,4,23,56,12,9,10),3,3)
d
det(d) 
solve(d)
c<- makeCacheMatrix(d)
cacheSolve(c)  ### returns inverse of matrix d
c$get()   ### returns matrix d
c$getinverse() ### returns inverse of d
