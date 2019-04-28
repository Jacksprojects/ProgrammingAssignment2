### Matrix caching and solving

##PART 1:
#The function below creates a special matrix which can cache it's inverse.
#First, let's create a small 4x4 matrix called 'First' which we'll use to test
#If the following two functions work!

first <- matrix(rnorm(1:16), ncol = 4) # This is an example matrix.
head(first) #Just check that it exists, always helpful.
solve(first) #This is the inverse of the example function. We'll use this as a test later.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


#Now that we've created the cached matrix we'll need a second function to fetch the matrix.
#The following function then computes the inverse of the matrix which is returned by the function above.
#If the inverse is already calculated, and the matrix is the same, the following function will fetch the inverse of this matrix from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

#Now that we have both functions, lets try them out. We'll use the output of the first function as the input for the second function:
#Remember, the example matrix that we called 'First' above will be our input into the first function.
second <- makeCacheMatrix(first)
cacheSolve(second)

#Lets check to see if this worked. Is the inverse to the first matrix equal to the output of our second function? Run the code and find out!
solve(first) == cacheSolve(second)
