
install.packages("Matrix")
library(Matrix)

# Ejemplo 1
A = matrix(1:9, ncol = 3)
A

# Usando librería Matrix
rankMatrix(A)


# Ejemplo 2
X = matrix(1:36, nrow = 6)
X

# Usando librería Matrix
rankMatrix(X)


# Ejemplo 3
M = matrix(sample(rnorm(20), 25, replace=TRUE), nrow=5)
M

rankMatrix(M)

# Ejemplo 4
# Definimos una matriz de numeros aleatorios normales
A = matrix(data = rnorm(12), ncol = 3)
A

# Rank con function qr() 
qr(A)$rank
rankMatrix(A)



#==================================
# Determinante
#=================================
# Ejemplo 1
A = matrix(c(1,0,0,1),2,2,byrow=T)
A

det(A)

# Ejemplo 2
Z = matrix(c(1,0,0,0,1,0,0,0,1),3,3,byrow=T)
Z

det(Z)

# Ejemplo 3
Q = matrix(c(4,6,6,9),2,2, byrow=T)
Q

det(Q)
