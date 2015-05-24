## Programming Assignment 2

## makeCacheMatrix - 
##The first function, makeCacheMatrix creates a special "matrix"
##set the value of the matrix
##get the value of the matrix
##set the value of the matrix
##get the value of the matrix

## Assumptions: From the assignment, all matrix included will be invertible, square
## Here was tested code
##  z <-matrix(c(1,1,2,3,1,3,1,2,4),3,3)
##  output <- makeCacheMatrix(z)
##  nextoutput <- cacheSolve(output)
##  nextoutput <- cacheSolve(output)
##   getting cached data

makeCacheMatrix <- function(x = matrix()) {
        # access later in cacheSolve. which is a cache object
        m <- NULL
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## cacheSolve - The following function calculates the mean of the special "matrix" created with the above function. However, it first checks to see if the matrix 
## has already been calculated. If so, it gets the mean from the cache and skips the computation.  Otherwise, it takes the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## normally i would add code to check if it's invertbiel, but they say make an assumption here
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        
        # if it was cachedd
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}


#Assumptions
#z <-matrix(c(1,1,2,3,1,3,1,2,4),3,3)


