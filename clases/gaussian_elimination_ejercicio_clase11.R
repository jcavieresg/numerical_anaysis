# Eliminación Gaussiana
# Ejemplo del Libro Howard, J. P. (2017). Computational Methods for Numerical Analysis with R. CRC Press.

# Creamos una matriz A
A = matrix(c(5, 5, 5, 8, 2, 2, 6, 5, 4), 3)
A

# El primer paso para resolver ecuaciones matemáticas es reducir una matriz a la forma escalonada mediante 
# la eliminación de Gaussiana. Una matriz está en forma escalonada por filas si la matriz cumple dos condiciones. 
# a) Las filas con solo valores de cero deben estar debajo de las filas con cualquier valor distinto de cero. 
# b) La primera entrada distinta de cero de cualquier otra fila debe estar a la derecha de la fila superior.


replacerow <- function (m, row1 , row2 , k) {
  m[row2 ,] <- m[row2 ,] + m[row1 ,] * k
  return (m)
}



# Algoritmo
gauss_elimina <- function (m) {
  count.rows <- nrow(m)
  count.cols <- ncol(m)
  piv <- 1
  for(row.curr in 1: count.rows ) {
    if(piv <= count.cols ) {
      i <- row.curr
      while (m[i, piv] == 0 && i < count.rows ) {
        i <- i + 1
        if(i > count.rows ) {
          i <- row.curr
          piv <- piv + 1
          if(piv > count.cols )
            return (m)
        }
      }
      if(i != row.curr )
        m <- swaprows(m, i, row.curr )
      for(j in row.curr : count.rows )
        if(j != row.curr ) {
          k <- m[j, piv] / m[row.curr , piv]
          m <- replacerow (m, row.curr , j, -k)
        }
      piv <- piv + 1
    }
  }
  return (m)
}

gauss_elimina(A)
