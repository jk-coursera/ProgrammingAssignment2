## Put comments here that give an overall description of what your
## functions do

## Wraps matrix 'x' with caching functionality.
makeCacheMatrix <- function(x = matrix()) {
    
    # Create variable for caching result of 'solve(x)'
    cache_solve <- NULL
    
    # Setter for matrix 'x'. Resets cached solution.
    set <- function(y) {
        x <<- y
        cache_solve <<- NULL
    }
    
    # Getter for matrix 'x'.
    get <- function() x
    
    # Sets cached solution.
    setsolution <- function(solution) cache_solve <<- solution
    
    # Gets cached solution.
    getsolution <- function() cache_solve
    
    # Return wrapped object.
    list(set = set, get = get,
         setsolution = setsolution,
         getsolution = getsolution)
}


## Computes the solution to matrix 'x' and caches the result.
## Subsequent calls to 'cacheSolve' will return the cached solution.
cacheSolve <- function(x, ...) {
    # Get the cached solution for matrix 'x'.
    solution <- x$getsolution()
    
    if(!is.null(solution))
    {
        message("cacheSolve: Returning cached solution.")
        return(solution)
    }
    
    # Get raw matrix from 'x', compute the solution, and cache the result.
    data <- x$get()
    solution <- solve(data)
    x$setsolution(solution)
    
    # Return the computed solution
    solution
}
