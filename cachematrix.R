## To clone the assignment i put the following line in git bash 
## git clone https://github.com/aresendiz/ProgrammingAssignment2.git
## makeCacheMatrix will set a matrix, then get the value of the matrix
## set a value of the inverse of the matrix Input (Inp) and get the value of this inverse


makeCacheMatrix <- function(x = matrix()) {
    Inverse<- NULL
    set <- function(Inp) {
        x <<- Inp
        Inverse<<- NULL
    }
    get <- function() x
    setsolve <- function(solve) Inverse<<- solve 
    getsolve <- function() Inverse
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## This function will return the inverse of a matrix
## using the solve stated function
## It starts checking if it has been computed already
## If it has not, it will compute the numbers 
## it it has then it gets to the cache and returns its value
## you can check if this value in in the cache since it returns
## the message: "Using cache data"

cacheSolve <- function(x, ...) {
    Inverse<- x$getsolve()
    if(!is.null(Inverse)) {
        message("Using cached data")
        return(Inverse)
    }
    data <- x$get()
    Inverse<- solve(data, ...)
    x$setsolve(Inverse)
    Inverse
}

## to check if this program works try the following files 
## in the console after running everything! 
## x<- matrix(1:6,3,2)
## Inverse<- makeCacheMAtrix(x)
## cacheSolve(Inverse)
## cacheSolve(Inverse)