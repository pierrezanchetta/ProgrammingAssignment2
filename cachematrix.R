# These two functions will create a matrix and return its inverse
# To use it, if M is a matrix, call "cacheSolve(makeCacheMatrix(M))"

makeCacheMatrix <- function(x = matrix())
{
# This function will create a special matrix which is a list
# Based on the same model as the template to cache a vector mean

	inv <- NULL

      set <- function(y)
	{
              x <<- y
              inv <<- NULL
      }

      get <- function() x
      setinverse <- function(inverse) inv <<- inverse
      getinverse <- function() inv

      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


cacheSolve <- function(x, ...)
{
	# This function will use the previous results to give the inverse of a matrix

	# First we get the inverse	
      inv <- x$getinverse()

	# If we receive something, calculation is over, we have the inverse
      if(!is.null(inv))
	{
            message("getting cached data")
            return(inv)
      }

	# Otherwise, we have to build the invese using "solve"

      data <- x$get()
      inv <- solve(data, ...)
      x$setinverse(inv)
      inv
}
