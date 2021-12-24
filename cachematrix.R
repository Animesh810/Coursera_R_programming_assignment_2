## These functions will invert a matrix and store their value in the cache
## This is done so that expensive invert operations can be skipped
## It does so by creating a special list of functions

## This function creates a list of functions
## These functions help in computing and storing the inverse in the cache for easy reitrieval
## It provides sort of an API to store the matrix and its inverse into cache

makeCacheMatrix <- function(x = matrix()) {
          m <- NULL
          set <- function(y) {
            x <<- y
            m <<- NULL
          }
          get <- function() x
          set_mat_invert <- function(inverted) m <<- inverted
          get_mat_invert <- function() m
          list(set = set, get = get,
               set_mat_invert = set_mat_invert,
               get_mat_invert = get_mat_invert)
  
}

## This the function that actually solves/inverts the matrix
## It takes an input of the special function list we have created in the above function
## It then checks if the matrix has not already been solved
## If it has, then it simply returns the value, else it solves it 
## After solving, it stores the value in the cache using the functions from the special list

cacheSolve <- function(x, ...) {
          ## Return a matrix that is the inverse of 'x'
          m <- x$get_mat_invert()
          if(!is.null(m)) {
            message("getting cached data")
            return(m)
          }
          data <- x$get()
          m <- solve(data, ...)
          x$set_mat_invert(m)
          m
}
