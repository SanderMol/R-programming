## Put comments here that give an overall description of what your
## functions do

## The function cacheSolve, supported by makeCacheMatrix, calculates the inverse of a matrix and stores it in cache.
## If the inverse of the matrix (with the exact same values) had already been calculated, it will retrieve it from cache.
## A lot of insights were gained from: https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-breakingDownMakeVector.md
## I may have over-commented it, but I used the comments for my own learning process. :)


## The function makeCacheMatrix is used to obtain matrices from the parent environment or save them to the parent environment.

makeCacheMatrix <- function(x = matrix()) {

	m <- NULL							# make the function environment start empty before creating the inverse matrix

	set <- function(y) {					# function to set new values to the parent environment in order to save them after executing makeCacheMatrix
		x <<- y						# save the y variable (which is the new inverse matrix) to the parent environment
		m <<- NULL						# reset the inverse matrix in the parent environment
	}

	get <- function() x					# function to get the original matrix from the parent environment
	setinverse <- function(inverse) m <<- inverse	# function to set the inverse matrix in the parent environment, where it is stored in variable m
	getinverse <- function() m				# function to get the inverse matrix from the parent environment, in case it exists
	list(set = set, get = get,				# create a list of names so the functions can be called from outside the makeCacheMatrix function
		setinverse = setinverse,
		getinverse = getinverse)
}

## The function cacheSolve calculates the inverse matrix or, if it already exists, retrieve the inverse matrix from cache.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()					# use the function to get the inverse matrix from the parent environment
        if(!is.null(m)) {					# check whether the inverse matrix variable exists and if so, return it
                message("getting cached data")
                return(m)
        }
        data <- x$get()						# get the original matrix from the parent environment and store it as 'data'
        m <- solve(data, ...)					# calculate the inverse of the matrix using the solve function
        x$setinverse(m)						# set the inverse matrix in the parent environment
        m								# print m, so that it is saved to the parent environment (which makes it redundant, actually)
}

## Extra code to test the functions
## This follows the test example of the link I shared above, but I added the crucial part of actually using the cache.

myMatrix <- matrix(c(2,3,3,4), nrow = 2, ncol = 2)	# get an initial matrix to work with
solve(myMatrix)							# shows what the inverse matrix would look like

aMatrix <- makeCacheMatrix(myMatrix)			# create an object environment containing the data of myMatrix
aMatrix$get()							# shows that the object environment saved the matrix data
aMatrix$getinverse()						# in a new environment, this should be NULL

myMatrix <- matrix(c(4,6,6,8), nrow = 2, ncol = 2)	# get a new matrix to work with, overwrites the old variable
solve(myMatrix)							# shows what the inverse matrix would look like

aMatrix$set(myMatrix)						# set the new matrix in the aMatrix object environment
cacheSolve(aMatrix)						# function calculates the inverse matrix and stores it in cache
cacheSolve(aMatrix)						# function notices that the matrix was not changed and retrieves the inverse matrix from cache
aMatrix$getinverse()						# gets the inverse matrix from cache
