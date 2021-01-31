## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## objeto de matriz para almacenar en caché
makeCacheMatrix <- function( matriz = matrix() ) {
  
  ## Declaración de variable a obtener
  inv <- NULL
  
  ## Método para almacenar la matriz
  set <- function( matrix ) {
    matriz <<- matrix
    inv <<- NULL
  }
  
  ## Método para obtener la matriz
  get <- function() {
    ## retorno de la matriz
    matriz
  }
  
  ## Método para almacenar la inversa de una matriz
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  
  ## Método para obtener la inversa de una matriz
  getInverse <- function() {
    ## retorno de la inversa
    inv
  }
  
  ## retorno de la lista de los métodos
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Write a short comment describing this function

##verificar si existen datos de la caché
cacheSolve <- function(x, ...) {
  ##se adquiere la variable de la inversa en caso de tener algún valor
  matriz <- x$getInverse()
  
  
  ##se comprueba si tiene algún valor
  if( !is.null(matriz) ) {
    message("getting cached data")
    return(matriz)
  }
  
  ## Se obtiene la matrix del objeto anterior
  data <- x$get()
  
  ## Se calcula la inversa de la matriz
  matriz <- solve(data) %*% data
  
  ## Se almacena la matriz 
  x$setInverse(matriz)
  
  ## Retorno de la inversa
  matriz
}
