## Pair of functions that catche the inverse of a matrix

## This function creates a special "matrix object that can cache its inverse.
### Inverse of a square matrix -> solve function

makeCacheMatrix <- function(x = matrix()) {
                m <-NULL
                set <-function(y){
                        x <<- y
                        m <<- NULL
                }
                get <-function() x
                setinverse <-function(solve) m <<- solve
                getinverse <-function() m
                list(set = set, get = get,
                     setinverse = setinverse,
                     getinverse = getinverse)
        }

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated(and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x = matrix(), ...) {
                m <- x$getinverse()
                if(!is.null(m)){
                        message("getting cached data")
                        return(m)
                }
                matrix <- x$get()
                m <- solve(matrix,...)
                x$setinverse(m)
                m
        }
        

 