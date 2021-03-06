#=========================================================================================
#                 MAESTRIA EN CIENCIAS EN INFORMATICA
#=========================================================================================
#     ALUMNO    : CARLOS DANIEL MARTINEZ ORTIZ
#     MATERIA   : ALMACENAMIENTO DE DATOS Y SU ADMINISTRACION
#     PROFESOR  : M.EN C. EDUARDO RENE RODRIGUEZ AVILA
#     TAREA     : NO. 8
#=========================================================================================
#=========================================================================================
# 1. Crea una Matriz para poder retener el c�lculo de su inversa
#=========================================================================================
makeCacheMatrix <- function(x = matrix()) 
{
  # Inicializa con nulos
  m <- NULL

  # Set de la Matriz
  set <- function(y)
  {
    x <<- y
    m <<- NULL
  }
  
  # Get de la Matriz
  get<-function() x
  
  # Set de la Inversa de la Matriz y la almacena en cach�
  setinversa <- function(solve) m <<- solve
  
  # Get de la Inversa de la Matriz del cach�
  getinversa <- function() m
  
  list(set = set, 
       get = get,
       setinversa = setinversa,
       getinversa = getinversa)
}

#=========================================================================================
# 2. Calcula la Matriz inversa del objeto "Matriz" especial que la funci�n makeCacheMatrix 
# devolver�. Si la Inversa ya ha sido calculada (y la matriz no ha cambiado), entonces
# �sta funci�n cacheSolve deber� devolver el valor ya calculado y retenido en el cache.
#=========================================================================================

cacheSolve <- function(x, ...) 
{
   # Obtiene la inversa de la Matriz
   m <- x$getinversa()

   # Si la matriz inversa se encuentra en cach�, se recupera.
   if(!is.null(m))
   {
     message("Obtiene datos del cach�")
     return(m)
   }
  
  # Obtiene la matriz 
  matrix <- x$get()
  # Regresa el valor de la matriz inversa
  m <- solve(matrix, ...)
  # Set de la matriz inversa en cach�
  x$setinversa(m)
  m
  }
#=========================================================================================
# 3. Crea matriz para ejecutar las funciones
#=========================================================================================
matrixA <- (matrix(sample(1:100,1000000,replace=T),1000,1000))

# Visualiza la Matriz
matrixA

#=========================================================================================
# 4. Calcula la matriz inversa(Sin almacenar en cach�)
#=========================================================================================
# Calcula la Matriz Inversa(Pero no la almacena en cach�)
solve(matrixA)

# Tiempo de procesamiento en calcular Matriz inversa (No recuperada de cach�)
system.time(solve(matrixA)) 

#=========================================================================================
# 5. EJECUCI�N DE LA FUNCI�N "makeCacheMatrix" con la matriz creada (Crea matriz)
#=========================================================================================
# Ejecuta funcion "makeCacheMatrix" para Crear matriz
x <- makeCacheMatrix(matrixA)

#=========================================================================================
# 6. EJECUCI�N DE LA FUNCI�N "cacheSolve" con la matriz creada (Calcula matriz inversa)
#=========================================================================================
# Ejecuta funcion "cacheSolve" para calcular la matriz inversa
#system.time(cacheSolve(x))
cacheSolve(x)

# Ejecuta funcion "cacheSolve" para calcular la matriz inversa (En esta 2da. ejecuci�n debe recuperarla de cach�)
#system.time(cacheSolve(x))
cacheSolve(x)

# Ejecuta funcion "cacheSolve" para Calcular la matriz inversa(En esta 3da. ejecuci�n debe recuperarla de cach�)
#system.time(cacheSolve(x))
cacheSolve(x)

