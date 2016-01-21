## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        minv <- NULL
        set <- function(y) {
                x <<- y
                minv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) minv <<- inverse
        getinverse <- function() minv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        cinv <- x$getinverse()
        if(!is.null(cinv)) {
                message("getting cached data.")
                return(cinv)
        }
        data <- x$get()
        cinv <- solve(data)
        x$setinverse(cinv)
        cinv
}

## Example:
## 
## Step 1 - Create data to store
##
## x = rbind(c(31, 34, 2), c(76, 1, 6), c(-3/4, 3/4, 8))
## i = makeCacheMatrix(x)
## 
## verify the data: 
## i$get
## 

## 
## Step 2 - Gettting the data with the cacheSolve function(1st run)
## 
## cacheSolve(i)
## 


## 
## Step 2 - Gettting the cached data with the cacheSolve function(2nd run)
##
## cacheSolve(i)
## 
## verify the message: getting cached data.
