# Ejemplo 1 (método biseccion) clase 8.

f = function(x) 1/4-exp(-(x^4))
v = seq(0,2,length=1000)
# Gráficar la funcion dentro de ese intervalo (v)
plt = function(f, a0, b0)
{
  plot(v,f(v), type = "l", ylim=c(-.5,.5), xlim=c(0,2),
       main=sprintf("Interval length = %f", (b0-a0)))
  abline(h=0)
  segments(a0,.1,a0,-.1,col=2,lty=1)
  points(a0,.1,col=2)
  points(a0,-.1,col=2)
  segments(b0,.1,b0,-.1,col=2,lty=1)
  points(b0,.1,col=2)
  points(b0,-.1,col=2)
  segments(mean(c(a0,b0)), .1, mean(c(a0,b0)), -.1, col=4, lty=1)
  points(mean(c(a0,b0)),.1,col=4)
  points(mean(c(a0,b0)),-.1,col=4)
}


# Ejemplo de la iteracion1 para el intervalo I
iter1 <- function(f, I)
{
  m = mean(I)
  if( f(I[1])*f(m) < 0 ) return( c(I[1],m) ) 
  else return( c(m, I[2]) )
}
# Valores de partida
I = c(0,2)
f = function(x) 1/4-exp(-(x^4))
# re type this several times to see bisection work
a0 = I[1]
b0 = I[2]
plt(f, a0, b0)
inter = iter1(f,I)
inter

# Método
# f is the function
# (a0,b0) es el intervalo
# the smaller tol is, the more accurate
bisec = function(f, a0, b0, tol){
  # Intervalo inicial
  I =  c(a0,b0)
  # Largo del intervalo
  L <- I[2]-I[1]
  while( L > tol )
  {
    # Punto medio
    m = mean(I)
    # Vemos si cambia el signo de f en la primera iteracion
    if( f(m)*f(I[1]) < 0 ) I = c(I[1],m) 
    else I = c(m,I[2])
    L <- I[2]-I[1]
  }
  return(mean(I))
}

f = function(x) 1/4-exp(-(x^4))

a0 = 5
b0 = 6

bisec(f, a0, b0, 1e-6)





#===========================================================================================
#                                    Método de Newton 
# (Howard, J. P. (2017). Computational Methods for Numerical Analysis with R. CRC Press.)
#===========================================================================================
f  = function(x) {x^2 - 1}
#fp = function(x) {2*x}
curve(f, xlim=c(-10,10), col='blue', lwd=2, lty=2, ylab='f(x)')
abline(h=0)
abline(v=0)


# Mediante funcion 'uniroot'
uniroot(f, c(2,3))  # Me da error.. por que?

f(1)
f(0)
f(2)
f(3)
f(4)
f(-0.5)
f(2)
uniroot(f, c(-0.5,2))



#===============================================================
#                 Algoritmo escrito en R
#===============================================================
f <- function(x) {x^2 - 1}

newton = function(f, a, b, tol = 1e-5, n = 1000) {
  require(numDeriv) # Librería para calcular la derivada de f(x)
  
  x0 = a # Valor inicial para el intervalo inferior
  k  = n # índice para el número de tteraciones
  
  # Verificicar si las aproximaciones dan como resultado 0
  fa = f(a)
  if (fa == 0.0) {
    return(a)
  }
  
  fb = f(b)
  if (fb == 0.0) {
    return(b)
  }
  
  # Partir con el proceso iterativo
  for (i in 1:n) {
    dx = genD(func = f, x = x0)$D[1] # Derivada de primer orden f'(x)
    x1 = x0 - (f(x0) / dx) # Calcular x1
    k[i] = x1 # Guardar x1
    # Una vez que la diferencia entre x0 y x1 sea lo suficientemente 
    #pequeña, genere los resultados.
    if (abs(x1 - x0) < tol) {
      root.approx <- tail(k, n = 1)
      res <- list('Approximacion' = root.approx, 'Iteraciones' = k)
      return(res)
    }
    # Si el método de Newton aún no ha alcanzado la convergencia, establezca x1 como x0 y continúe
    x0 = x1
  }
  print('No se encontro la solucion en el numero de iteraciones dadas')
}

# Aplicamos el algoritmo creado
newton(f, -0.5, 2, tol = 1e-6, n = 100)





# Ejemplo 2
# f(x) = e^2x - x - 6 = 0
f = function(x) {exp(2 * x) - x - 6}

curve(f, col = 'blue', lty = 2, lwd = 2, xlim=c(-5,5), ylim=c(-5,5), ylab='f(x)')
abline(h=0)
abline(v=0)

uniroot(f, c(0, 2))
newton(f, 0, 2, tol = 1e-6, n = 1000)



