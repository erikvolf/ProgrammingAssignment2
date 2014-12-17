## Thise functions is used to cache the inverse of a matrix so it do not have to
## be solve every time the inverse of a matrix is needed. But only when the data
## in the matrix has changed.
## There is no control of the matrix, so it is assumed that the matrix is a square
## and the inverse can be solved




## The function makeCacheMatrix create a special "matrix" which have 4 functions inside

## It is initialized by assign it to a variable 
##    ex. myMatrix<-makeCacheMatrix() or myMatrix<-makeCacheMatrix(x) where x is a matrix

## Use the get function to get the matrix
##    ex. myMatrix$get()

## Use the set function to change the matrix
##    ex. myMatrix$set(y)  where y is a matrix

## The 2 last functions setinv and getinv should not be use directly. But is used by the cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- NULL           ## Set inverseMatrix to empty at inittializing

    set <- function(y){             ## Set function where the matrix is set to a new value and inverseMatrix is set to empty
        x <<- y
        inverseMatrix <<- NULL
    }

    get <- function() x             ## Get function that return the matrix.

    setinv <- function(invx) inverseMatrix <<- invx   ## Setinv function that assign a new value to inverseMatrix.

    getinv <- function() inverseMatrix                ## Getinv function that return the value of inverseMatrix.

    list( set = set,   ## List of functions in makeCacheMatrix.
        get = get,
        setinv = setinv,
        getinv = getinv)
    
} ## end of makeCacheMatrix function


## The function cacheSolve returns the inverse of a matrix
## First time it is called or if the matrix has changed it will solve, store and return the inverse matrix
## else it will return the stored inverse matrix and save time
## It is called by ex. cacheSolve(myMatrix)

cacheSolve <- function(x, ...) {
        
    invx <- x$getinv()          ## Get the cached inverse matrix
    
    if(is.null(invx)) {                ## If "inverse matrix" is empty
        message("execute solve(data)")   ## (help messaged)
        data <- x$get()                  ## get the matrix
        invx <- solve(data, ...)         ## inverse matrix with solve()
        x$setinv(invx)                   ## store "invers matrix"
    }
    
    else message("getting cached data")   ## (help messaged)
    
    invx  ## return "inverse matrix"
    
} ## end of cacheSolve function
