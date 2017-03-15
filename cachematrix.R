## Functions to create and cache the matrix and its calculated inverse


## builds set of functions and returns them w/in a list to parent envirn.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    ## initialized two objects, x and m
    
    ## define functions for objects of type makeCacheMatrix()
    set <- function(y) {
        x <<- y  ## Assign the input argument
        m <<- NULL  ## Assign the value of NULL to clear any previous cached value
    }
    get <- function() x  ## retrieves x from the parent environment
    setinvm <- function(solve) m <<- solve  ## assign the input argument to the value of m in the parent environment
    getinvm <- function() m  ## retrieves m value
    
    ## Create new object by returning a list with named elements
    list(set = set, get = get,
         setinvm = setinvm,
         getinvm = getinvm)
}


## Checks cached data or calculates inverse and returns matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
    ## First, attempt to retrieve a cached inv matrix from the object x
    m <- x$getinvm()
     if(!is.null(m)) {
          message("getting cached data")
            return(m)
     }
    ## if nothing in cache, then sovle for the inverse matrix
    data <- x$get()
    m <- solve(data, ...)
    x$setinvm(m)
    m
}
