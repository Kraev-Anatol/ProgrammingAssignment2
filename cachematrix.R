makeCacheMatrix <- function(x = matrix()) {
        inversMatrix <- NULL  ## reserves a variable for an inverse matrix
        
        getMatrix <- function() x 
        
        setInvers <- function(invM) inversMatrix <<- invM   ## saves the inverse matrix in the cache 
                
        getInvers <- function() inversMatrix                 ## receives the inverse matrix from the cache
        
        ## transfer of matrixes to cacheSolve
        
        list(getMatrix = getMatrix, setInvers = setInvers, getInvers = getInvers)
}

cacheSolve <- function(x, ...) {
        inversMatrix <- x$getInvers()        ## receives the inverse matrix from the cache
        
        if(!is.null(inversMatrix)) {                       
                message("Getting Cached Inverse Matrix")    
                return(inversMatrix)        ## return of an inverse matrix if it already exists                        
        }
        
        inversMatrix <- solve(x$getMatrix())  ## calculation of an inverse matrix
        x$setInvers(inversMatrix)             ## saves the inverse matrix in the cache 
        return(inversMatrix)
}

## code testing

M <- cbind(c(1,0,1),c(0,1,2),c(0,0,1))
M

cacheMatrix <- makeCacheMatrix(M)
cacheMatrix$getMatrix()
cacheMatrix$setInvers(NULL)
cacheMatrix$getInvers()

cacheSolve(cacheMatrix)
cacheSolve(cacheMatrix)
cacheSolve(cacheMatrix)
