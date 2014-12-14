## The following functions allow to cache the value of the inverse of a matrix
## as it has a very high computational cost. The basic idea is to use the special
## assignment operator <<- (or ->>) used to store a variable in a parent environment
## and the search of it is made through parent evironments: if found then its value (if
## not locked) is redefined otherwise the assignment takes place in the global environment.


## The following function create a special matrix object which is basically a list
## containing functions as elements: set store the matrix in a parent environment,
## get return its value, setinverse store the inverse of the given matrix in a
## parent environment and finally getinverse retreives the inverse from the cached value.
## This function can be called in two ways:
## > myMatrix <- makeCacheMatrix() # creates an empty matrix object
## > myMatrix$set(matrix(c(19,64,8,26),2,2)) # sets the matrix elements
## or in a second way:
## > myMatrix <- makeCacheMatrix(matrix(c(19,64,8,26),2,2)) # creates and sets a matrix object to its values

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                  x <<- y
                  inv <<- NULL
        }
        get <- function() x
        setinverse <- function(invsolve) invsolve ->> inv
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  
}


## The following function retreives the value of the inverse if it has been already
## cached, otherwise first check if the matrix is invertible computing its determinant,
## if it is NaN or zero then the matrix is not invertible and a message is returned,
## otherwise calls the solve function and then stores the value of the inverse in the
## cache in order to retreive it for future use.
## To cache the inverse matrix we can call:
## > myInvMatrix <- cacheSolve(myMatrix)
## and if we call it again we can see it is retreived from the cache:
## > myInvMatrix <- cacheSolve(myMatrix)
## to check the operation is correct:
## > myMatrix$get() %*% myInvMatrix # in this case the matrices can be commuted
## The result is the identity matrix.

## Semper et semper oremus.

cacheSolve <- function(x=matrix(), ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                  message("getting cached data")
                  return(inv)
        }
        matdata <- x$get()
        matdet <- det(matdata)
        if(is.nan(matdet) || (0 == matdet)) {
                  message("matrix is not invertible")
        }
        else {
                  inv <- solve(matdata, ...)
                  x$setinverse(inv)
                  inv
        }
}
