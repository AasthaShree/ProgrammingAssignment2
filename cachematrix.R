##Cache the inverse of a matrix

##1. Function to create 'special' matrix to cache its inverse
## assuming symmetric matrix otherwise function will throw error.
makeCacheMatrix <- function(mtx = matrix()){ 
  inv <- NULL
  set <- function(tmp){ 
    mtx <<- tmp
    inv <<- NULL
  }
  #get the matrix
  get <- function() mtx 
  #set the value of inverse of the matrix using solve function
  setinv <- function(solve) inv <<- solve
  #get the value of inverse of the matrix
  getinv <- function() inv 
  #create a list of the assigned values
  list(set = set, get = get, setinv = setinv,getinv = getinv)               
}

#2. Function to comput inverse of 'special' matrix and, in case already calculated, retrieve the cached value

cacheSolve <- function(mtx = matrix(), ...) {
  inv <- mtx$getinv()
  #check whether the inverse has been computed
  if(!is.null(inv)){                        
    message("getting cached data")
    return(inv)
  }
  inv_mat <- mtx$get()
  #else, compute the inverse matrix
  inv <- solve(inv_mat, ...)               
  mtx$setinv(inv)
  #print the inverse of the matrix
  inv                                       
}

## Sample execution
##mtx= cbind(c(7, 9), c(9, 7))
##run function makeCacheMatrix
##a = makeCacheMatrix(mtx)
##a$get()
##[,1] [,2]
##[1,]    7    9
##[2,]    9    7
##run the cacheSolve function to cache the matrix inverse
##cacheSolve(a)
##[,1]     [,2]
##[1,] -0.21875  0.28125
##[2,]  0.28125 -0.21875
##Rerun the function to check if caching is occuring.
##cacheSolve(a)
##getting cached data
##[,1]     [,2]
##[1,] -0.21875  0.28125
##[2,]  0.28125 -0.21875