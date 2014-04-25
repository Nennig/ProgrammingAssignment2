## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix())
{
  m <- NULL
  set <- function(y)
    {                                             # define the set function
      x <<- y
      m <<- NULL
    }
  get <- function() x                             # define the get function
  setsolve <- function(solve) m <<- solve         # define the setsolve function for this special matrix
  getInverse <- function() m                      # define tge getInveerse function for this special matrix
  list(set = set, get = get,
       setsolve = setsolve,
       getInverse = getInverse)
}



## Write a short comment describing this function

cacheSolve <- function(x, ...)
{
  m <- x$getInverse()                             # try to retrieve the cached matrix
  if(!is.null(m))
      {                                           # check if cache exists
        message("getting cached data")            # inform user that this is from the cache
        return(m)                                 # return the cached matrix, no need for computation
      }
  data <- x$get()                                 # at this point there is no existing cache
  m <- solve(data, ...)                           # lets compute the inverse
  x$setsolve(m)                                   # save the inverse matrix in the cache
  m                                               # return result to user
}