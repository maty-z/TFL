###### Configuracion inicial e importación de librerías
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(latex2exp)

source("./scr/f_estimaciones_v5.R")

##### Funciones auxiliares ####
#para calcular los cuantiles poblacionales y 
#los estimados de manera paramétrica y no paramétrica 

quant <- function(p, p_umbral, lambda, umbral) {
  -1/lambda*log((1-p)/(1-p_umbral)) + umbral
}

p_umbral_est <- function(muestra,est.u) {
  1-sum(muestra>est.u)/length(muestra)
}

cuants_p_est.experiment <- function(experiment_name,cuantiles) {
  # Leo resultados obtenidos y las muestras
  result_path <- paste("./outputs/results_", experiment_name,".dat",sep="")
  sample_path <- paste("./data/muestras_", experiment_name,".dat",sep="")
  
  df.result <- read.table(result_path,header = T)
  muestra <- matrix(scan(sample_path,quiet = T),byrow = T,nrow = df.result$replicates)
  
  # Estimo p_umbral a partir del umbral estimado
  est.p_umbral <- mapply(p_umbral_est,muestra = asplit(muestra,1),est.u = df.result$est.u)
  
  # Estimo los cuantiles no paramétricos
  cuant.no_params <- t(sapply(asplit(muestra,1),quantile,probs = cuantiles))
  
  #Agrego a los resultados previos las nuevas estimaciones
  cbind(df.result,est.p_umbral,unlist(cuant.no_params))
}

###### Cargo resultados de experimentos en un data.frame #####
list.files("./outputs/",full.names = TRUE) |>
  lapply(read.table,header = TRUE) |>
  do.call(rbind,args = _) -> df_estimaciones

# Para calcular los cuantiles sobre las muestras ya utilizadas previamente
#necesito importarlas, usar los resultados obtenidos previamente y luego 
#hacer las estimaciones correspondientes y 
#luego mantener los resultados para poder hacer los análisis correspondientes
list.experiments_names <- unique(df_estimaciones$name)
list.sample_names <- paste("./data/muestras_", list.experiments_names,".dat",sep="")

cuantiles = c(0.8,0.85,0.9,0.95,0.99,0.999,0.9999,0.999999,0.999999999)
# Ejecuto la funcion anterior para cada uno de los experimentos y 
#para cada uno de los cuantiles
inicio <- Sys.time()
lapply(list.experiments_names,cuants_p_est.experiment,cuantiles = cuantiles ) |>
  do.call(rbind,args = _) -> df_est_p_umbral
fin <- Sys.time() - inicio
print(fin)

## Guardo resultados intermedios
#write.table(df_est_p_umbral,"./outputs_cuantiles/df_results_cuantiles.dat",row.names = FALSE)

# Estimo los cuantiles paramétricamente. 
#Cargo los resultados intermedios. Y luego hago los cálculos correspondientes.
df_est_p_umbral <- read.table("./outputs_cuantiles/df_results_cuantiles.dat", header = T, check.names = F)
cuantiles_parametricos <- sapply(cuantiles,quant,p_umbral = df_est_p_umbral$est.p_umbral,
                                lambda = df_est_p_umbral$est.lamb,
                                umbral = df_est_p_umbral$est.u)
# Renombro para mantener consistencia en la nomenclatura según lo obtenido al
#calcular los cuantiles no paramétricos
colnames(cuantiles_parametricos)<-c("80%","85%","90%","95%","99%","99.9%","99.99%","99.9999%","100%")

## Guardo resultados de cuantiles paramétricos.
#write.table(cuantiles_parametricos,"./outputs_cuantiles/cuantiles_parametricos.dat",row.names = FALSE)

# Cargo resultados obtenidos
cuantiles_parametricos <- read.table("./outputs_cuantiles/cuantiles_parametricos.dat",header = T, check.names = F)
cuantiles_no_parametricos <- read.table("./outputs_cuantiles/df_results_cuantiles.dat", header = T, check.names = F)

# Modifico nombres para facilitar posterior análisis
colnames(cuantiles_parametricos) <- paste("param_", c(0.8,0.85,0.9,0.95,0.99,0.999,0.9999,0.999999,0.999999999),sep="")
colnames(cuantiles_no_parametricos)[13:21] <- paste("noparam_",c(0.8,0.85,0.9,0.95,0.99,0.999,0.9999,0.999999,0.999999999),sep="")

# Cruzo datos
df<-cbind(cuantiles_no_parametricos,cuantiles_parametricos)


#### Transformo resultados: wide format a long format ####
df |>
  pivot_longer(
    cols = contains("param"),
    names_to = c("Param","cuantil"),
    names_pattern = "(.*)_(.*)",
    names_transform = c(cuantil = ~as.double(.x),
                        Param = ~ case_match(.x,
                                             "param" ~ "Paramétrico",
                                             "noparam" ~ "No paramétrico")),
    values_to = c("Valor")
  ) -> df.long



#Calculo y ploteo medidas de resumen considerando el tamaño de muestra
# Repito los calculos para cada lambda
df.long |> mutate(
  factor_exp_lambda = case_match(
    exp_lambda, 
    c(4,19) ~ "Continuo", 
    .default = as.factor(exp_lambda)
  ))|>
  group_by(size,p_umbral,Param,cuantil,factor_exp_lambda) |>
  summarise(EMAE = mean(abs(Valor/quant(cuantil,p_umbral,exp_lambda,1))), 
            MAD = median(abs(Valor/quant(cuantil,p_umbral,exp_lambda,1))),
            IQR = IQR(Valor/quant(cuantil,p_umbral,exp_lambda,1))
            ) -> df.resumen.size

#lambda = 0.5
subset(df.resumen.size, 
       (cuantil %in% c(0.95, 0.99, 0.9999, 0.999999)) & 
         (factor_exp_lambda == "0.5")
) |>
  ggplot() + 
  aes(x = as.factor(size), y=MAD, colour = Param, group = Param) +
  geom_line() + geom_point() +
  geom_hline(yintercept = 1, linetype = "dashed", alpha = 0.5) +
  facet_grid(p_umbral~cuantil,labeller = label_both) +
  labs(color = "Modelo", 
       x = "Tamaño muestra", 
       y = TeX("Mediana $\\frac{\\hat {p}_{cuantil}}{p_{cuantil}}$"),
       title = TeX("$\\lambda = 0.5$"))
#ggsave("./outputs_charts/size_mad_cuantiles_4niveles_lambda_05.png",width = 32, height = 18, units = "cm")

#lambda = 1
subset(df.resumen.size, 
       (cuantil %in% c(0.95, 0.99, 0.9999, 0.999999)) & 
         (factor_exp_lambda == "1")
) |>
  ggplot() + 
  aes(x = as.factor(size), y=MAD, colour = Param, group = Param) +
  geom_line() + geom_point() +
  geom_hline(yintercept = 1, linetype = "dashed", alpha = 0.5) +
  facet_grid(p_umbral~cuantil,labeller = label_both) +
  labs(color = "Modelo", 
       x = "Tamaño muestra", 
       y = TeX("Mediana $\\frac{\\hat {p}_{cuantil}}{p_{cuantil}}$"),
       title = TeX("$\\lambda = 1$"))
#ggsave("./outputs_charts/size_mad_cuantiles_4niveles_lambda_1.png",width = 32, height = 18, units = "cm")

#lambda = 10
subset(df.resumen.size, 
       (cuantil %in% c(0.95, 0.99, 0.9999, 0.999999)) & 
         (factor_exp_lambda == "10")
) |>
  ggplot() + 
  aes(x = as.factor(size), y=MAD, colour = Param, group = Param) +
  geom_line() + geom_point() +
  geom_hline(yintercept = 1, linetype = "dashed", alpha = 0.5) +
  facet_grid(p_umbral~cuantil,labeller = label_both) +
  labs(color = "Modelo", 
       x = "Tamaño muestra", 
       y = TeX("Mediana $\\frac{\\hat {p}_{cuantil}}{p_{cuantil}}$"),
       title = TeX("$\\lambda = 10$"))
#ggsave("./outputs_charts/size_mad_cuantiles_4niveles_lambda_10.png",width = 32, height = 18, units = "cm")

#lambda = 30
subset(df.resumen.size, 
       (cuantil %in% c(0.95, 0.99, 0.9999, 0.999999)) & 
         (factor_exp_lambda == "30")
) |>
  ggplot() + 
  aes(x = as.factor(size), y=MAD, colour = Param, group = Param) +
  geom_line() + geom_point() +
  geom_hline(yintercept = 1, linetype = "dashed", alpha = 0.5) +
  facet_grid(p_umbral~cuantil,labeller = label_both) +
  labs(color = "Modelo", 
       x = "Tamaño muestra", 
       y = TeX("Mediana $\\frac{\\hat {p}_{cuantil}}{p_{cuantil}}$"),
       title = TeX("$\\lambda = 30$"))
#ggsave("./outputs_charts/size_mad_cuantiles_4niveles_lambda_30.png",width = 32, height = 18, units = "cm")

#lambda = 50
subset(df.resumen.size, 
       (cuantil %in% c(0.95, 0.99, 0.9999, 0.999999)) & 
       (factor_exp_lambda == "50")
) |>
  ggplot() + 
  aes(x = as.factor(size), y=MAD, colour = Param, group = Param) +
  geom_line() + geom_point() +
  geom_hline(yintercept = 1, linetype = "dashed", alpha = 0.5) +
  facet_grid(p_umbral~cuantil,labeller = label_both) +
  labs(color = "Modelo", 
       x = "Tamaño muestra", 
       y = TeX("Mediana $\\frac{\\hat {p}_{cuantil}}{p_{cuantil}}$"),
       title = TeX("$\\lambda = 50$"))
#ggsave("./outputs_charts/size_mad_cuantiles_4niveles_lambda_50.png",width = 32, height = 18, units = "cm")

#lambda = continuo
subset(df.resumen.size, 
       (cuantil %in% c(0.95, 0.99, 0.9999, 0.999999)) & 
         (factor_exp_lambda == "Continuo")
) |>
  ggplot() + 
  aes(x = as.factor(size), y=MAD, colour = Param, group = Param) +
  geom_line() + geom_point() +
  geom_hline(yintercept = 1, linetype = "dashed", alpha = 0.5) +
  facet_grid(p_umbral~cuantil,labeller = label_both) +
  labs(color = "Modelo", 
       x = "Tamaño muestra", 
       y = TeX("Mediana $\\frac{\\hat {p}_{cuantil}}{p_{cuantil}}$"),
       title = TeX("$\\lambda = Continuo$"))
#ggsave("./outputs_charts/size_mad_cuantiles_4niveles_lambda_cont.png",width = 32, height = 18, units = "cm")
