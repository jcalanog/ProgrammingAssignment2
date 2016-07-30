

## Author 	: Jae Calanog
## Date		: July 28 2016
## Course	: R Programming
## Work		: Assignment #2 Lexical Scoping. 

## Description: 
## This short script is intended to demonstrate R's lexical programming through 
## matrix inversion caching. A "special" matrix-object is generated that is able
## to cache its inverse. 

## There are 2 function necessary for this assignment. 
## makeCacheMatrix - this function creates a special "matrix" object that can cache its inverse. 

## Expected return: Matrix with ability to: 
##  - function with setter/getters value of the matrix. 
##  - function with setter/getter value of the inverse matrix. 
makeCacheMatrix <- function(x = matrix()) {

	## here we make [i] as the inverse. 
	i <- NULL
	set <- function(y) 
	{
		x <<- y
		i <<- NULL 
	}
	get <- function() x

	## the two special functions relating to inverse matrix. 
	setinverse <- function(i_value) i <<- i_value
	getinverse <- function() i 
	## return object of the [makeCacheMatrix] function. 
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve - this function computes the inverse of the special "matrix" returned by the 
## [makeCacheMatrix]. If the inverse has already been calculated (and the matrix has not
## changed), then [cacheSolve] should retrive the inverse from the cache. 

## important assumption: matrix supplied is ALWAYS invertible. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if (!is.null(i)) 
        {
        	message("getting cached inverse data")
        	return(i)
        }
        data <- x$get()
        i <- solve(data,...)
        x$setinverse(i)
        i
}