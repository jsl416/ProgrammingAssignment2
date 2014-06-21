## ---------------------------------------------------------------------------------------------------------------------
## 2014-06-22 04:27
## Written By: JS Lee
## Purpose of this R code: This R code is to calculate an inverse of a sqaure matrix.
##                         When the inverse is calculated alreadly, the cached inverse will be returned which saves caculation time.
##----------------------------------------------------------------------------------------------------------------------
## Firstly, "makecacheMatrix" function will create a matrix, which will be saved into a matrix variable. 
## Then, it will calculate the inverse of the matrix using "Sovle" function. The result will be set(cached) it as m.
## Finally, The list of four calculation results will be returned, which are set, get, setsolve, getsolve.
## (Input matrix should only be a square matrix.)

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL                               # initialize with NULL
        set <- function(y) {                    
                x <<- y                         
                m <<- NULL
        }
        get <- function() x                     # return x, which is the matrix
        setsolve <- function(solve) m <<- solve # solve function is applied to get the inverse
        getsolve <- function() m                # return calculated m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}
 
## "cacheSolve" function checks getsolve to see if there is any value. 
## If there is, it will return m value, which is the inverse of the matrix.
## Otherwise, it will calculate the inverse of the matrix.

cacheSolve <- function(x, ...) {
        m <- x$getsolve() ## Return a matrix that is the inverse of 'x'
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()         #This will retrieve the matirx.
        m <- solve(data, ...)   #It solves to get the inverse of the matrix.
        x$setsolve(m)           #Set the inverse matrix as m
        m                       #Return m value
}
