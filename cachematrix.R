## The cachematrix module provides a set of functions to save the inverses of
## matrices in cache in order to save computational time

## This function initializes cache and returns a cache matrix object
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    
    ## Test if cache exists, if not, initialize cache
    
    if (!exists(as.character(substitute(cache))))
    cache <<- NULL
    
    ##  Create list object of two matrices x
    
    cacheM <- cbind(list(x),list(x))
    
    return(cacheM)

}


## This function checks if the inverse of the matrix is stored in cache.
## If the inverse is not found in cache, the matrix is solved and a new
## matrix/inverse pair is attached to cache

cacheSolve <- function(x, ...) {
    
            ## Initialize cache matrix object
            
            cacheM <- makeCacheMatrix(x)
            
            isCached <- FALSE
            
            ## Look up matrix in cache to find inverse
            
            if (!is.null(cache)) {
                
                for (i in seq(1,length(cache)/2)) {
                    
                    ## Check if cache matrix is equal to list objects in cache
                    ## If TRUE, return the corresponding inverse
                    
                    if (isTRUE(all.equal(cacheM[[1,1]],cache[[i,1]]))) {
                        
                        cacheM[[1,2]] <- cache[[i,2]]
                        isCached <- TRUE
                        
                        message("matrix found in cache!")
                        
                        ## Return a matrix that is the inverse of 'x'
                        
                        return(cacheM[[1,2]])
                        
                    }
                    
                    if (isTRUE(all.equal(cacheM[[1,1]],cache[[i,2]]))) {
                
                        cacheM[[1,2]] <- cache[[i,1]]
                        isCached <- TRUE
                        
                        message("matrix found in cache!")
                        
                        ## Return a matrix that is the inverse of 'x'
                        
                        return(cacheM[[1,2]])
                    }
                    
                }
                
            } 
            
            ## If inverse is not in cache solve matrix and
            ## add matrix and its inverse to cache
            
            if (!isCached) {
                
                cacheM[[1,2]] <- solve(cacheM[[1,1]])
                
                ## Attach chache matrix to cache
                
                cache <<- rbind(cache,cacheM)
            
                message("new matrix attached to cache!")
                
                ## Return a matrix that is the inverse of 'x'
                
                return(cacheM[[1,2]])
                
            }
        
}
