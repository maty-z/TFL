#### Script de funciones para realizar estimaciones ####

# Distribucion de exceso empirica en funcion del umbral u y la muestra
emp_Fu <- function(u,muestra) {
  return(ecdf(muestra[muestra>=u]-u))
}

# Estimo el lambda con maxima verosimilitud 
#para los excesos de la muestra con umbral u
lambda_emv <- function(u,muestra) {
  exceso <- muestra[muestra>=u]
  return(length(exceso)/sum(exceso-u)) # lambda_emv = 1/promedio(x-u)|x>u
}

# Distancia norma supremo entre la empirica de los excesos y la exp
#con parametro dado por el lambda de maxima verosimilitud de: excesos-u
#Aclaracion: Este resultado es el estadistico utilizado por el test 
#Kolomogorov-Smirnov
dsup_lambda_u <- function(u,muestra) {
  pexp_rate = lambda_emv(u,muestra)
  exceso <- muestra[muestra>=u]
  return(ks.test(exceso-u,"pexp",rate = pexp_rate)$statistic)
}

# Funcion a optimizar:
# alpha_pen y n_pen son los exponentes del argumento para penalizacion
f_obj <- function(u,muestra,alpha_pen,n_pen,c) {
  return(dsup_lambda_u(u,muestra) + c/length(muestra)^alpha_pen*u^n_pen)
}

# Dada una muestra evalúa la función objetivo en los u candidatos a umbral,
#luego devuelve el que minimiza la f_obj
u_est <- function(muestra, n_pen, alpha_pen, c, qinf = 0.5) {
  u_interval <- c(quantile(muestra, probs = qinf),
                  quantile(muestra, probs = 0.99)
                 )

  u_candidatos <- muestra[
                          muestra > u_interval[1]
                        & muestra < u_interval[2]
                         ]

  # La función objetivo cambia de comportamiento en los valores muestrales
  #Evalúo la funcion objetivo dentro de los candidatos a umbral, 
  #y luego me quedo con el que minimiza la función.
  f_obj.values <- lapply(u_candidatos,
                         f_obj, 
                         muestra = muestra,
                         n_pen = n_pen,
                         alpha_pen = alpha_pen,
                         c= c)
  argmin <- which.min(f_obj.values)
  return(u_candidatos[argmin])
}
