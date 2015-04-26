## Put comments here that give an overall description of what your
## functions do

## This first function creates a special matrix object that caches its inverse
## produces a list containing a funciton to
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {#subfunction 1 that changes the matrix x stored in the main function.
                x <<- y
                i <<- NULL
        }
        get <- function() x #subfunction 2 that returns the matrix x stored in the main function. No input required.
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,#stores the functions in a list
             setinverse = setinverse,
             getinverse = getinverse)
}
#Explanation of the above function:
#1. Set the value of the matrix
#We only need to use this subfunction if we want to change the matrix.
#The second line of the subfunction x <<- y replaces the matrix x with the matrix y in the main function makeCacheMatrix.
#If we wrote x<- y then the matrix x would have been replaced by y within the environment of the set function.
#i<<- NULL resets the value of the mean to NULL - we don't need the inverse of the old matrix any more!
#The new inverse is calculated through the function cacheSolve
#2. Get the value of the matrix

#The next two subfunctions don't calculate the inverse. THey simpy store it's input value in a variable i
#into the main function makeCacheMatrix("setinverse") and return it ()
#3. set the value of the inverse
#4. get the value of the inverse


## This second function computes the inverse of the special matrix returned by the first function.
##The input of the second function is the object where makeCacheMatrix is stored.	

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached matrix")
                return(i) #returns i, in the case that it is not Null, then returning i ends the main function.
        }#if NULL, we move on to the 'else' component of the function:
        data <- x$get() #gets the matrix stored with makeCacheMatrix, assigns it to variable named'data'
        i <- solve(data, ...) #calculates the inverse of the 'data' matrix
        x$setinverse(i) #this call assigns the value generated in the above line to the variable of the object we stored a call to makeCacheMatrix on.
        i
}
