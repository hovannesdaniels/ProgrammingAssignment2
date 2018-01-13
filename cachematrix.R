## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The makeCacheMatrix function will return a list that will
## set the matrix, get the matrix, set the inverse and get the inverse.
## This list is used as the input to the function cacheSolve()

##################################################################
## Keeping makevector in the R script to model cacheSolve       ##
## makeVector <- function(x = numeric()) {                      ##
##        m <- NULL                                             ##
##        set <- function(y) {                                  ##
##                x <<- y                                       ##
##                m <<- NULL                                    ##
##        }                                                     ##
##        get <- function() x                                   ##
##        setmean <- function(mean) m <<- mean                  ##
##        getmean <- function() m                               ##
##        list(set = set, get = get,                            ##
##             setmean = setmean,                               ##
##             getmean = getmean)                               ##
##}                                                             ##
##################################################################

makeCacheMatrix <- function(x = matrix()) {
        ## Assume x is always an invertible matrix (square)
        
        inv = NULL
        set = function(y) {
                # use `<<-` to assign a value to an object in an environment
                # different from the current environment.
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, 
             setinv=setinv,
             getinv=getinv)
}


## Write a short comment describing this function

## The cacheSolve function will return the inverse of
## the original matrix (X) input to makeCacheMatrix()

##################################################################
## Keeping cachemean in the R script to model cacheSolve        ##
## cachemean <- function(x, ...) {                              ##
##        m <- x$getmean()                                      ##
##        if(!is.null(m)) {                                     ##
##                message("getting cached data")                ##
##                return(m)                                     ##
##        }                                                     ##
##        data <- x$get()                                       ##
##        m <- mean(data, ...)                                  ##
##        x$setmean(m)                                          ##
##        m                                                     ##
##}                                                             ##
##################################################################

cacheSolve <- function(x, ...) {
        
        
        inv = x$getinv()
        
        # if the inverse has already been calculated
        if (!is.null(inv)){
                # get it from the cache and skips the computation. 
                message("getting cached data")
                return(inv)
        }
        
        # otherwise, calculates the inverse 
        mat.data = x$get()
        inv = solve(mat.data, ...)
        
        # sets the value of the inverse in the cache via the setinv function.
        x$setinv(inv)
        
        return(inv)
}