## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        
        # if im does not exist yet, create im and initialize to NULL
        if (!exists("im")) {
                im <<- NULL
        }
        
        # if refm does not exist yet, create refm and initialize to NULL
        if (!exists("refm")) {
                refm <<- NULL
        }
        
        # if m does not exist yet, create m and initialize to NULL
        if (!exists("m")) {
              m <<- NULL
        }
        
        #setter and getter functions
        setmat <- function(mat) m <<- mat
        getmat <- function() m
        setimat <- function(imat) im <<- imat
        getimat <- function() im
        setrefmat <- function(refmat) refm <<- refmat
        getrefmat <- function () refm

        #the function makeCacheMatrix must take an argument
        if (!missing(x)) {
                #make sure an argument is passed into the makeCacheMatrix function
                if (class(x) == "matrix") {
                        #only square matrices can be inverted
                        if (nrow(x) == ncol(x)) {
                                if (!identical(x,m)) {
                                        printmat <- cat("the source matrix currently in the cache is ", getmat())
                                        message (printmat)
                                        setmat(x) #assign x to m
                                        printmat <- cat("the new source matrix just stored into cache is ", getmat())
                                        message (printmat)
                                } else {
                                        printmat <- "the new source matrix is identical to source matrix already in the cache"
                                        message(printmat)
                                }
                                list(setmat = setmat, getmat = getmat, setimat = setimat, getimat = getimat, setrefmat = setrefmat, getrefmat = getrefmat)
                        } else {
                                message ("the matrix in the argument is not square")     
                        }
                } else {
                        message ("the argument is not of class matrix")
                }
        } else {
                print ("no agrument was passed into the makeCacheMatrix function")
        }     
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        if (is.null(x)) {
                message ("argument is either not a square matrix or of class matrix")       
        } else {
                i <- x$getimat()
                if(!is.null(i)) {
                        if (identical(x$getrefmat(),x$getmat())) {
                                #retrieve the inverse matrix from the cache if the source matrix is unchanged
                                message("retrieving the cached inverse matrix when the source matrix is unchanged")
                                #i <- x$getimat()                        
                        } else {
                                #compute the inverse matrix when the source matrix has changed
                                message("computing the inverse matrix when the source matrix has changed")
                                x$setimat(solve(x$getmat()))
                                x$setrefmat(x$getmat())
                                #i <- x$getimat()
                        }
                } else {
                        #compute the inverse matrix for the first time 
                        message("computing the inverse matrix for the first time")
                        x$setimat(solve(x$getmat()))
                        x$setrefmat(x$getmat())
                        #i <- x$getimat()
                }
                
                i <- x$getimat()
                message("the inverse matrix is ")
                #printinvmat <- cat("the inverse matrix is ", i)
                #print(printinvmat)
                return (i)
        }                
}
                
                
                
 
