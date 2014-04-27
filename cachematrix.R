## This file holds 2 functions - makeCacheMatrix and cacheSolve.
## Their purpose is to take a matrix, find it's inverse, and then return it
## If the inverse already exists, it will pull it from the cache, 
## if it doesn't, it will calculate it using the solve() function.


## makeCacheMatrix creates a special vector that does operations and puts them in a list
## 1. set the value of the vector
## 2. get the value of the vector
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) 
{
        i <- NULL
        set <- function(y)
        {
                x <<- y
                i <<-NULL
        }
        
        get <- function() x
        
        setinv <- function(solve) i <<- solve
        
        getinv <- function () i
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
## cacheSolve creates the inverse of the special vector
## but first it checks to see if that special vector has
## already been calculated.  If it has, then it retrieves
## it and skips the calculation.

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if(!is.null(i))
        {
                message("getting inverse of matrix")
                return(i)
        }
        data <- x$get()
        i <- solve(data,...)
        x$setinv(i)
        i
}


