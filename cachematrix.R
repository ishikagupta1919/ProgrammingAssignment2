##We have to write 2 functions -> "makeCacheMatrix" and "cacheSolve" 
## for  caching the inverse of a matrix. 
## If cache is present, then computation is faster

##makeCacheMatrix is a function which creates a special "matrix" object that can
##cache its inverse for the input (which is an invertible square matrix)

makeCacheMatrix <- function(x = matrix(sample(1:100,9),3,3)) {
  ##S is the result
  ##x is input matrix
  ##initialize s to Null
  
  
  s <- NULL
  set <- function(y) {
    x <<- y
    ##assigning from different envt
    s <<- NULL
  }
  
  ##creating a special vector wgich contains mean & values
  
  get <- function() x ##checks cache and returns
  setsolve <- function(solve) s <<- solve ##sets value of mean in cache
  getsolve <- function() s 
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve is a function which computes the inverse of the special "matrix"
##returned by makeCacheMatrix above. If the inverse has already been calculated
##(and the matrix has not changed), then the cachesolve should retrieve the
##inverse from the cache

cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting inversed matrix")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}