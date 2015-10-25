## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# The first function, makeVector creates a special "vector", which is 
# really a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse matrix
# 4. get the value of the invesr matrix


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y            
        m <<- NULL         # create/ reset m on global environmental.
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function
# The following function calculates the inverse matrix  created
# with the above function. However, it first checks to see if the inverse  has
# already been calculated. If so, it gets the inverse matrix from the cache and
# skips the computation. Otherwise, it calculates the inverse of the data of the data 
# and sets the value of the inverse matrix  in the cache via the setinv function.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)  # compute inverse matrix
    x$setinv(m)
    m
}



## test functions
m <- makeCacheMatrix()
m$set(matrix(c(0,2,2,0),2,2))
m$get()


cacheSolve(m)

cacheSolve(m)


