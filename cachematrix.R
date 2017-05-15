##Cache the inverse of a matrix

#1. Function to create 'special' matrix to cache its inverse
makeCacheMatrix <- function(mtx = matrix()){ #assuming symmetric matrix
  inv <- NULL
  set <- function(tmp){ 
    mtx <<- tmp
    inv <<- NULL
  }
  get <- function() mtx                     #get the matrix
  setinv <- function(solve) inv <<- solve   #set the value of inverse of the matrix using solve function
  getinv <- function() inv                  #get the value of inverse of the matrix
  list(set = set, get = get,                #create a list of the assigned values
       setinv = setinv,
       getinv = getinv)
}

#2. Function to comput inverse of 'special' matrix and, in case already calculated, retrieve the cached value
cacheSolve <- function(mtx = matrix(), ...) {
  inv <- mtx$getinv()
  if(!is.null(inv)){                        #check whether the inverse has been computed
    message("getting cached data")
    return(inv)
  }
  inv_mat <- mtx$get()                      
  inv <- solve(inv_mat, ...)                #else, compute the inverse matrix
  mtx$setinv(inv)
  inv                                       #Return a matrix that is the inverse of mtx
}