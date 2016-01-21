## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL   # Cache m 
  set <- function(y) { # Stocke la matrice m qui prend ici la valeur de x mais la matrice m reste cachée
    x <<- y
    m <<- NULL
  }
  get <- function() x # Renvoie la valeur de la matrice stockée : m
  setmatrix <- function(matrix) m <<- matrix
  getmatrix <- function() m   #Renvoie m
  list(set = set, get = get, 
       setmatrix = setmatrix,
       getmatrix = getmatrix)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m) #Il retourne la valeur de l'inverse de m déja calculé
  }
  data <- x$get()
  m <- solve(data, ...) #On calcule l'inverse de la matrice à partir de la commande solve
  x$setmatrix(m)
  m
}