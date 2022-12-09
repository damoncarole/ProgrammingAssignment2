
#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.



makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {                      # set the value of the matrix
    x <<- y
    m <<- NULL
  }
  get <- function() x                       # get the value of the matrix
  setinv <- function(inverse) m <<- inverse #set the value of the inversed matrix
  getinv <- function() m                    #get the value of the inversed matrix
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#            If the inverse has already been calculated (and the matrix has not changed), 
#            then the cachesolve should retrieve the inverse from the cache.

cachesolve <- function(x, ...) {
  m <- x$getinv()                   #gets the inverse of the matrix
  if(!is.null(m)) {                 #checks to see if the matrix has already been inversed
    message("getting cached data")
    return(m)
  }
  data <- x$get()                  
  m <- solve(data, ...)             #returns its inverse
  x$setinv(m)                       #sets the inversed matrix in the cache via the setinv function.
  m
}

