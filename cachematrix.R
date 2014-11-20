## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        
        # if m does not exist yet, create m and initialize to NULL
        if (!exists("m")) {
              m <<- NULL
        }
        
        #setter and getter functions
        setmat <- function(mat) m <<- mat
        getmat <- function() m
        setimat <- function(imat) im <<- imat
        getimat <- function() im


        #make sure an argument is passed into the makeCacheMatrix function
        if (class(x) == "matrix") {
                #only square matrices can be inverted
                if (nrow(x) == ncol(x)) {
                        if (!identical(x,m)) {
                                printvect <- cat("the matrix curently in the cache is ", getmat())
                                print (printmat)
                                setmat(x) #assign x to m
                                printmat <- cat("the new matrix just stored into cache is ", getmat())
                                print (printmat)
                        } else {
                                printmat <- "the new matrix is identical to stored matrix"
                                print (printmat)
                        }
                } else {
                        print ("the matrix in the argument is not square")     
                }

        } else {
                print ("the argument is not of class matrix")
        }

        list(setmat = setmat, getmat = getmat, setimat = setimat, getimat = getimat)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
