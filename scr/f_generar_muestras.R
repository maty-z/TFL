#### Script de funciones para generar muestras ####

##########################
# Genero una bernoulli de prob p. Si es 1, entonces tomo una muestra uniforme,
#si no es 1, tomo una muestra de la exponencial.
tomar_muestra.unif_exp <- function(p_umbral,
                                   u_min,
                                   u_max,
                                   exp_lambda) {
  aux <- rbinom(n = 1, size = 1, prob = p_umbral)
  if (aux == 1) {
    z <- runif(1, min = u_min, max = u_max)
  } else {
    z <- rexp(1, exp_lambda) + u_max
  }
  return(z)
}
# Replico el paso para tomar muestras de tamaño size
generar_muestra.unif_exp <- function(size, ...) {
  dist_params <- list(...)
  replicate(size, do.call(tomar_muestra.unif_exp, dist_params))
}

# Algoritmo más rápido para tomar muestras.
#Fue implementado posteriormente al inicio de los experimentos.
generar_muestra.unif_exp_2 <- function(size,
                                       p_umbral,
                                       u_min,
                                       u_max,
                                       exp_lambda) {
  bernoullies <- rbinom(n = size, size = 1, prob = p_umbral)
  ifelse(bernoullies == 1,
    runif(n = size, min = u_min, max = u_max),
    rexp(n = size, rate = exp_lambda)+u_max)
  }
##########################

##########################
# Genero una bernoulli de prob p. Si es 1, entonces tomo una muestra normal
#truncada, si no es 1, tomo una muestra de la exponencial.
tomar_muestra.normal_exp <- function(p_umbral,
                                     norm_max,
                                     mu,
                                     sigma2,
                                     exp_lambda) {
  aux <- rbinom(n = 1, size = 1, prob = p_umbral)
  if (aux == 1) {
    pnorm_max <- pnorm(norm_max, mean = mu, sd = sqrt(sigma2))
    z <- qnorm(runif(1, max = pnorm_max), mean = mu, sd = sqrt(sigma2))
  } else {
    z <- rexp(1, exp_lambda) + norm_max
  }
  return(z)
}


# Replico el paso para tomar muestras de tamaño size
generar_muestra.normal_exp <- function(size, ...) {
  dist_params <- list(...)
  replicate(size, do.call(tomar_muestra.normal_exp, dist_params))
}

# Algoritmo más rápido para tomar muestras.
#Fue implementado posteriormente al inicio de los experimentos.
generar_muestra.normal_exp_2 <- function(size,
                                       p_umbral,
                                       norm_max,
                                       mu,
                                       sigma2,
                                       exp_lambda) {
  bernoullies <- rbinom(n = size, size = 1, prob = p_umbral)
  pnorm_max <- pnorm(norm_max, mean = mu, sd = sqrt(sigma2))
  ifelse(bernoullies == 1,
    qnorm(runif(n = size, max = pnorm_max), mean = mu, sd = sqrt(sigma2)),
    rexp(n = size, rate = exp_lambda)+norm_max)
  }
##########################

# Atajo para con una misma funcion generar muestras según distintos modelos
#de tamaño size
generar_muestra <- function(size, modelo = "unif_exp" , ...) {
  muestra_params <- c(size = size, list(...))
  switch(modelo,
    unif_exp = do.call(generar_muestra.unif_exp_2, muestra_params),
    normal_exp = do.call(generar_muestra.normal_exp_2, muestra_params)
    )
}

# Algoritmo más rápido para tomar muestras.
#Fue implementado posteriormente al inicio de los experimentos.
generar_muestra_2 <- function(size, modelo = "unif_exp" , ...) {
  muestra_params <- c(size = size, list(...))
  switch(modelo,
         unif_exp = do.call(generar_muestra.unif_exp, muestra_params),
         normal_exp = do.call(generar_muestra.normal_exp, muestra_params)
  )
}
