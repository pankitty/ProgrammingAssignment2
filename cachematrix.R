
########## create the functions: cacheSolve and makeCacheMatrix

rm(list=ls())

cacheSolve<-function(x = matrix()) {
        m <- NULL
        if_square<-function(x) {
                if (dim(x)[1]==dim(x)[2]) 
                        "False"
                else "True"
        }
        get <- function() x
        setinverse <- function(inverse_matrix) m <<- inverse_matrix      ### store the result "m" out of the loop by using "<<-"
        getinverse <- function() m
        
        list(get = get, setinverse = setinverse, getinverse = getinverse,if_square=if_square)        
}

makeCacheMatrix<-function(x,...) {
        
        #### check if there is a stored results.
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        #### load data
        data <- x$get()
        
        #### check if it is a squared matrix
        p<- x$if_square(data)
        if(p == "True") {
                message("getting non square matrix")
                return(p)
        }
        
        #### inverse matrix
        m <- solve(data)
        
        #### store the inverse out of the loop.
        x$setinverse(m)
        
        #### return inversed matrix
        m
}





#########################################################################
########### run the functions

#### create a random matrix 
matrix_example<- replicate(5, rnorm(5)) 

#### pass the data into cacheSolve function and create a data and function combination.
x<-cacheSolve(matrix_example)

#### run the final function. 
makeCacheMatrix(x)

##objects(cacheSolve())
